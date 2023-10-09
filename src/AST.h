//===----------------------------------------------------------------------===//
// Abstract Syntax Tree (aka Parse Tree)
//===----------------------------------------------------------------------===//#include "llvm/IR/LLVMContext.h" how to find files

#ifndef AST_HPP
#define AST_HPP

#pragma once

#include <string>
#include <vector>
#include <cstdio>
#include <map>
#include <memory>

#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Verifier.h"
#include "llvm/IR/LegacyPassManager.h"
#include "llvm/Transforms/InstCombine/InstCombine.h"
#include "llvm/Transforms/Scalar.h"
#include "llvm/Transforms/Scalar/GVN.h"
#include "llvm/Transforms/Utils.h"
#include "llvm/Support/TargetSelect.h"
#include "KaleidoscopeJIT.h"

using namespace llvm;

#define USE_JIT 0

static std::unique_ptr<LLVMContext> TheContext;
static std::unique_ptr<Module> TheModule;	// 顶层容器，存放所有函数和全局变量
static std::unique_ptr<IRBuilder<>> Builder;
static std::map<std::string, AllocaInst*> NamedValues;	// 当前作用域的变量与对应的Value
#if USE_JIT
static std::unique_ptr<legacy::FunctionPassManager> TheFPM;
static std::unique_ptr<llvm::orc::KaleidoscopeJIT> TheJIT;
#endif
static ExitOnError ExitOnErr;

Function* getFunction(const std::string& FuncName);

// TODO 是否有可能有运算符二义性？
static std::map<char, int> BinopPrecedence = {
	{'=', 2},
	{'<', 10},
	{'+', 20},
	{'-', 20},
	{'*', 40},
	{'/', 40},
};

/// CreateEntryBlockAlloca - Create an alloca instruction in the entry block of
/// the function.  This is used for mutable variables etc.
/// 为函数创建局部变量
static AllocaInst *CreateEntryBlockAlloca(Function *TheFunction, StringRef VarName)
{
	IRBuilder<> TmpB(&TheFunction->getEntryBlock(), TheFunction->getEntryBlock().begin());
	return TmpB.CreateAlloca(Type::getDoubleTy(*TheContext), 0, VarName);
}

/// ExprAST - Base class for all expression nodes.
class ExprAST
{
public:
	virtual ~ExprAST() {}

	// 负责生成AST节点的IR代码
	virtual Value* Codegen() = 0;
};

/// NumberExprAST - Expression class for numeric literals like "1.0".
class NumberExprAST : public ExprAST
{
	double Val;

public:
	NumberExprAST(double val) : Val(val) {}
	Value* Codegen() override;
};

/// VariableExprAST - Expression class for referencing a variable, like "a".
class VariableExprAST : public ExprAST
{
	std::string Name;

public:
	VariableExprAST(const std::string &name) : Name(name) {}
	Value* Codegen() override;
	std::string GetName() const { return Name; }
};

/// VarExprAST - Expression class for var/in
class VarExprAST : public ExprAST
{
	std::vector<std::pair<std::string, std::unique_ptr<ExprAST>>> VarNames;
	std::unique_ptr<ExprAST> Body;

public:
	VarExprAST(std::vector<std::pair<std::string, std::unique_ptr<ExprAST>>> VarNames, std::unique_ptr<ExprAST> Body)
		: VarNames(std::move(VarNames)), Body(std::move(Body)) {}
	Value *Codegen() override;
};

/// BinaryExprAST - Expression class for a binary operator. 二元运算符，比如加减乘除
class BinaryExprAST : public ExprAST
{
	char Op;
	std::unique_ptr<ExprAST> LHS, RHS;

public:
	BinaryExprAST(char op, std::unique_ptr<ExprAST> lhs, std::unique_ptr<ExprAST> rhs)
		: Op(op), LHS(std::move(lhs)), RHS(std::move(rhs)) {}
	Value* Codegen() override;
};

/// UnaryExprAST - Expression class for a unary operator.
class UnaryExprAST : public ExprAST
{
	char Opcode;
	std::unique_ptr<ExprAST> Operand;

public:
	UnaryExprAST(char Opcode, std::unique_ptr<ExprAST> Operand)
		: Opcode(Opcode), Operand(std::move(Operand)) {}

	Value *Codegen() override;
};

/// CallExprAST - Expression class for function calls.
class CallExprAST : public ExprAST
{
	std::string Callee;
	std::vector<std::unique_ptr<ExprAST>> Args;

public:
	CallExprAST(const std::string &callee, std::vector<std::unique_ptr<ExprAST>> args)
		: Callee(callee), Args(std::move(args)) {}
	Value* Codegen() override;
};

/// IfExprAST - Expression class for if/then/else
class IfExprAST : public ExprAST
{
	std::unique_ptr<ExprAST> Cond;
	std::unique_ptr<ExprAST> Then;
	std::unique_ptr<ExprAST> Else;

public:
	IfExprAST(std::unique_ptr<ExprAST> Cond, std::unique_ptr<ExprAST> Then, std::unique_ptr<ExprAST> Else)
		: Cond(std::move(Cond)), Then(std::move(Then)), Else(std::move(Else)) {}
	Value* Codegen() override;
};

/// ForExpr - Expression class for for ... in
class ForExprAST : public ExprAST
{
	std::string VarName;
	std::unique_ptr<ExprAST> Start;
	std::unique_ptr<ExprAST> End;
	std::unique_ptr<ExprAST> Step;
	std::unique_ptr<ExprAST> Body;

public:
	ForExprAST(const std::string &VarName, std::unique_ptr<ExprAST> Start, std::unique_ptr<ExprAST> End, std::unique_ptr<ExprAST> Step, std::unique_ptr<ExprAST> Body)
		: VarName(VarName), Start(std::move(Start)), End(std::move(End)), Step(std::move(Step)), Body(std::move(Body)) {}
	Value* Codegen() override;
};

/// PrototypeAST - This class represents the "prototype" for a function,
/// which captures its name, and its argument names (thus implicitly the number
/// of arguments the function takes).
/// 函数原型/函数接口/函数声明
/// prototypeexpr ::= identifier '(' identifier* ')'
/// foo(x y)
class PrototypeAST
{
	std::string Name;
	std::vector<std::string> Args;
	bool IsOperator;
	unsigned Precedence;	// 当且仅当是binary operator时才有效

public:
	PrototypeAST(const std::string &Name, const std::vector<std::string> Args, bool IsOperator = false, unsigned Prec = 0)
		: Name(Name), Args(std::move(Args)), IsOperator(IsOperator), Precedence(Prec) {}
	Function* Codegen();
	const std::string GetName() const { return Name;}

	bool IsUnaryOp() const { return IsOperator && Args.size() == 1; }
	bool IsBinaryOp() const { return IsOperator && Args.size() == 2; }

	char GetOperatorName() const {
		assert(IsUnaryOp() || IsBinaryOp());
		return Name[Name.size() - 1];
	}

	unsigned GetBinaryPrecedence() const {
		assert(IsBinaryOp());
		return Precedence;
	}
};

/// FunctionAST - This class represents a function definition itself.
/// 函数本身，包括函数原型和函数体
class FunctionAST
{
	std::unique_ptr<PrototypeAST> Proto;
	std::unique_ptr<ExprAST> Body;

public:
	FunctionAST(std::unique_ptr<PrototypeAST> proto, std::unique_ptr<ExprAST> body)
		: Proto(std::move(proto)), Body(std::move(body)) {}

	Function* Codegen();
};

#pragma region 报错辅助

// 区分了三种Error函数，返回值类型不同
std::unique_ptr<ExprAST> LogError(const char *Str)
{
	fprintf(stderr, "LogError: %s\n", Str);
	return nullptr;
}

std::unique_ptr<PrototypeAST> LogErrorP(const char *Str)
{
	LogError(Str);
	return nullptr;
}

std::unique_ptr<FunctionAST> LogErrorF(const char *Str)
{
	LogError(Str);
	return nullptr;
}

Value* LogErrorV(const char *Str) {
	LogError(Str);
	return nullptr;
}

#pragma endregion

#pragma region Codegen()实现

Value* NumberExprAST::Codegen()
{
	return ConstantFP::get(*TheContext, APFloat(Val));
}

Value* VariableExprAST::Codegen()
{
	// Look this variable up in the function.
	AllocaInst *A = NamedValues[Name];
	if (!A)
		return LogErrorV("Unknown variable name");
	// Load the value.
	return Builder->CreateLoad(A->getAllocatedType(), A, Name.c_str());
}

Value* VarExprAST::Codegen()
{
	std::vector<AllocaInst *> OldBindings;

	Function *TheFunction = Builder->GetInsertBlock()->getParent();

	// Register all variables and emit their initializer.
	for (unsigned i = 0, e = VarNames.size(); i != e; ++i)
	{
		const std::string &VarName = VarNames[i].first;
		ExprAST *Init = VarNames[i].second.get();
		// Emit the initializer before adding the variable to scope, this prevents
		// the initializer from referencing the variable itself, and permits stuff
		// like this:
		//  var a = 1 in
		//    var a = a in ...   # refers to outer 'a'.
		Value *InitVal;
		if (Init)
		{
			InitVal = Init->Codegen();
			if (!InitVal)
				return nullptr;
		}
		else
		{ // If not specified, use 0.0.
			InitVal = ConstantFP::get(*TheContext, APFloat(0.0));	// 局部变量默认值为0
		}

		AllocaInst *Alloca = CreateEntryBlockAlloca(TheFunction, VarName);
		Builder->CreateStore(InitVal, Alloca);

		// Remember the old variable binding so that we can restore the binding when
		// we unrecurse.
		OldBindings.push_back(NamedValues[VarName]);

		// Remember this binding.
		NamedValues[VarName] = Alloca;
	}

	// Codegen the body, now that all vars are in scope.
	Value *BodyVal = Body->Codegen();
	if (!BodyVal)
		return nullptr;

	// Pop all our variables from scope.
	for (unsigned i = 0, e = VarNames.size(); i != e; ++i)
		NamedValues[VarNames[i].first] = OldBindings[i];

	// Return the body computation.
	return BodyVal;
}

Value* BinaryExprAST::Codegen()
{
	if (Op == '=') {
		// Assignment requires the LHS to be an identifier.
		// This assume we're building without RTTI because LLVM builds that way by
		// default.  If you build LLVM with RTTI this can be changed to a
		// dynamic_cast for automatic error checking.
		VariableExprAST *LHSE = static_cast<VariableExprAST *>(LHS.get());
		if (!LHSE)
			return LogErrorV("destination of '=' must be a variable");
		// Codegen the RHS.
		Value *Val = RHS->Codegen();
		if (!Val)
			return nullptr;

		// Look up the name.
		Value *Variable = NamedValues[LHSE->GetName()];
		if (!Variable)
			return LogErrorV("Unknown variable name");

		Builder->CreateStore(Val, Variable);
		return Val;
	}

	Value *L = LHS->Codegen();
	Value *R = RHS->Codegen();
	if (L == nullptr || R == nullptr)
		return nullptr;

	switch (Op)
	{
	case '+':
		return Builder->CreateFAdd(L, R, "addtmp");
	case '-':
		return Builder->CreateFSub(L, R, "subtmp");
	case '*':
		return Builder->CreateFMul(L, R, "multmp");
	case '/':
		return Builder->CreateFDiv(L, R, "divmp");
	case '<':
	{
		L = Builder->CreateFCmpULT(L, R, "cmptmp");
		return Builder->CreateUIToFP(L, Type::getDoubleTy(*TheContext), "booltmp");
	}
	default:
		break;
	}

	// If it wasn't a builtin binary operator, it must be a user defined one. Emit
	// a call to it.
	Function *F = getFunction(std::string("binary") + Op);
	assert(F && "binary operator not found!");

	Value *Ops[2] = {L, R};
	return Builder->CreateCall(F, Ops, "binop");
}

Value* UnaryExprAST::Codegen()
{
	Value *OperandV = Operand->Codegen();
	if (OperandV == nullptr)
		return nullptr;

	Function *F = getFunction(std::string("unary") + Opcode);
	if (!F)
		return LogErrorV("Unknown unary operator");

	return Builder->CreateCall(F, OperandV, "unop");
}

Value* IfExprAST::Codegen()
{
	Value *CondV = Cond->Codegen();
	if (!CondV)
		return nullptr;

	CondV = Builder->CreateFCmpONE(CondV, ConstantFP::get(*TheContext, APFloat(0.0)), "ifcond");

	Function *TheFunction = Builder->GetInsertBlock()->getParent();

	// Create blocks for the then and else cases.  Insert the 'then' block at the
	// end of the function.
	BasicBlock *ThenBB = BasicBlock::Create(*TheContext, "then", TheFunction);
	BasicBlock *ElseBB = BasicBlock::Create(*TheContext, "else");
	BasicBlock *MergeBB = BasicBlock::Create(*TheContext, "ifcont");

	Builder->CreateCondBr(CondV, ThenBB, ElseBB);

	// Emit then value.
	Builder->SetInsertPoint(ThenBB);

	Value *ThenV = Then->Codegen();		// Then的具体代码在Codegen过程中，插入到了ThenBB里面
	if (!ThenV)
		return nullptr;

	Builder->CreateBr(MergeBB);
	// Codegen of 'Then' can change the current block, update ThenBB for the PHI.
	ThenBB = Builder->GetInsertBlock();

	// Emit else block.
	TheFunction->insert(TheFunction->end(), ElseBB);
	Builder->SetInsertPoint(ElseBB);

	Value *ElseV = Else->Codegen();		// Else的具体代码在Codegen过程中，插入到了ElseBB里面
	if (!ElseV)
		return nullptr;

	Builder->CreateBr(MergeBB);
	// Codegen of 'Else' can change the current block, update ElseBB for the PHI.
	ElseBB = Builder->GetInsertBlock();

	// Emit merge block.
	TheFunction->insert(TheFunction->end(), MergeBB);
	Builder->SetInsertPoint(MergeBB);
	PHINode *PN = Builder->CreatePHI(Type::getDoubleTy(*TheContext), 2, "iftmp");	// 在MergeBB中，创建一个PHINNode，这个Node会作为函数的返回值

	PN->addIncoming(ThenV, ThenBB);
	PN->addIncoming(ElseV, ElseBB);
	return PN;
}

Value* ForExprAST::Codegen()
{
	// Make the new basic block for the loop header, inserting after current
	// block.
	Function *TheFunction = Builder->GetInsertBlock()->getParent();

	// Create an alloca for the variable in the entry block.
	AllocaInst *Alloca = CreateEntryBlockAlloca(TheFunction, VarName);

	// Emit the start code first, without 'variable' in scope.
	Value *StartVal = Start->Codegen();
	if (!StartVal)
		return nullptr;

	// Store the value into the alloca.
	Builder->CreateStore(StartVal, Alloca);

	BasicBlock *LoopBB = BasicBlock::Create(*TheContext, "loop", TheFunction);

	// Insert an explicit fall through from the current block to the LoopBB.
	Builder->CreateBr(LoopBB);

	// Start insertion in LoopBB.
	Builder->SetInsertPoint(LoopBB);

	// Within the loop, the variable is defined equal to the PHI node.  If it
	// shadows an existing variable, we have to restore it, so save it now.
	AllocaInst *OldVal = NamedValues[VarName];	// 先把重名的旧的Value提取出来（比如for之前已经定义过i，使用for中的局部变量暂时覆盖它）
	NamedValues[VarName] = Alloca;				// 让VarName可以被Body使用

	// Emit the body of the loop.  This, like any other expr, can change the
	// current BB.  Note that we ignore the value computed by the body, but don't
	// allow an error.
	if (!Body->Codegen())		// 经过这一行Body的相关IR代码已经插到了LoopBB里面
		return nullptr;

	// Emit the step value.
	Value *StepVal = nullptr;
	if (Step)
	{
		StepVal = Step->Codegen();	// 经过这一行Step的相关IR代码已经插到了LoopBB里面
		if (!StepVal)
			return nullptr;
	}
	else
	{
		// If not specified, use 1.0.
		StepVal = ConstantFP::get(*TheContext, APFloat(1.0));
	}

	// Compute the end condition.
	Value *EndCond = End->Codegen();
	if (!EndCond)
		return nullptr;

	// Reload, increment, and restore the alloca.  This handles the case where
	// the body of the loop mutates the variable.
	Value *CurVar = Builder->CreateLoad(Alloca->getAllocatedType(), Alloca, VarName.c_str());
	Value *NextVar = Builder->CreateFAdd(CurVar, StepVal, "nextvar");
	Builder->CreateStore(NextVar, Alloca);

	// Convert condition to a bool by comparing non-equal to 0.0.
	EndCond = Builder->CreateFCmpONE(EndCond, ConstantFP::get(*TheContext, APFloat(0.0)), "loopcond");

	// Create the "after loop" block and insert it.
	BasicBlock *AfterBB = BasicBlock::Create(*TheContext, "afterloop", TheFunction);

	// Insert the conditional branch into the end of LoopEndBB.
	Builder->CreateCondBr(EndCond, LoopBB, AfterBB);

	// Any new code will be inserted in AfterBB.
	Builder->SetInsertPoint(AfterBB);

	// Restore the unshadowed variable.
	if (OldVal)
		NamedValues[VarName] = OldVal;		// 把局部变量覆盖的Val还回去
	else
		NamedValues.erase(VarName);

	// for expr always returns 0.0.
	return Constant::getNullValue(Type::getDoubleTy(*TheContext));
}

static std::map<std::string, std::unique_ptr<PrototypeAST>> FunctionProtos;	// 缓存函数定义

Function* getFunction(const std::string& FuncName)
{
	if (auto *F = TheModule->getFunction(FuncName))	// 当前模块已经有Func了，直接返回
		return F;

	auto FI = FunctionProtos.find(FuncName);
	if (FI != FunctionProtos.end())
	{
		return FI->second->Codegen();	// 如果找不到，但是有缓存的Proto，重新创建一个Function
	}

	return nullptr;
}

Value* CallExprAST::Codegen()
{
	Function* CalleeF = getFunction(Callee);
	if (CalleeF == nullptr)
		return LogErrorV("Unknown function referenced");

	if (CalleeF->arg_size() != Args.size())
		return LogErrorV("Incorrect # arguments passed");

	std::vector<Value *> ArgsV;
	for (unsigned i = 0, e = Args.size(); i != e; ++i)
	{
		ArgsV.push_back(Args[i]->Codegen());
		if (ArgsV.back() == nullptr)
			return nullptr;
	}

	return Builder->CreateCall(CalleeF, ArgsV, "calltmp");
}

Function* PrototypeAST::Codegen()
{
	std::vector<Type *> Doubles(Args.size(), Type::getDoubleTy(*TheContext));	// 参数列表全是double类型

	FunctionType *FT = FunctionType::get(Type::getDoubleTy(*TheContext), Doubles, false);	// 返回值类型、形参列表

	Function *F = Function::Create(FT, Function::ExternalLinkage, Name, TheModule.get());

	// Set names for all arguments.
	unsigned Idx = 0;
	for (auto &Arg : F->args())
		Arg.setName(Args[Idx++]);

	return F;
}

Function *FunctionAST::Codegen()
{
	auto &P = *Proto;

	FunctionProtos[Proto->GetName()] = std::move(Proto);	// 把proto的名称和proto实例缓存

	Function *TheFunction = getFunction(P.GetName());

	if (!TheFunction)
		return nullptr;

	if (P.IsBinaryOp())
		BinopPrecedence[P.GetOperatorName()] = P.GetBinaryPrecedence();

	// Create a new basic block to start insertion into.
	BasicBlock *BB = BasicBlock::Create(*TheContext, "entry", TheFunction);
	Builder->SetInsertPoint(BB);

	// Record the function arguments in the NamedValues map.
	NamedValues.clear();
	for (auto &Arg : TheFunction->args())
	{
		// Create an alloca for this variable.
		AllocaInst *Alloca = CreateEntryBlockAlloca(TheFunction, Arg.getName());

		// Store the initial value into the alloca.
		Builder->CreateStore(&Arg, Alloca);

		// Add arguments to variable symbol table.
		NamedValues[std::string(Arg.getName())] = Alloca;
	}


	if (Value *RetVal = Body->Codegen())
	{
		// Finish off the function.
		Builder->CreateRet(RetVal);

		// Validate the generated code, checking for consistency.
		verifyFunction(*TheFunction);
#if USE_JIT
		// Optimize the function.
		TheFPM->run(*TheFunction);
#endif

		return TheFunction;
	}
	// Error reading body, remove function.
	TheFunction->eraseFromParent();

	if (P.IsBinaryOp())
		BinopPrecedence.erase(P.GetOperatorName());
	return nullptr;
}

#pragma endregion

#endif
