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
#include "llvm/Support/TargetSelect.h"
#include "KaleidoscopeJIT.h"

using namespace llvm;

static std::unique_ptr<LLVMContext> TheContext;
static std::unique_ptr<Module> TheModule;	// 顶层容器，存放所有函数和全局变量
static std::unique_ptr<IRBuilder<>> Builder;
static std::map<std::string, Value*> NamedValues;	// 当前作用域的变量与对应的Value
static std::unique_ptr<legacy::FunctionPassManager> TheFPM;
static std::unique_ptr<llvm::orc::KaleidoscopeJIT> TheJIT;
static ExitOnError ExitOnErr;

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

public:
	PrototypeAST(const std::string &name, const std::vector<std::string> args)
		: Name(name), Args(std::move(args)) {}
	Function* Codegen();
	const std::string GetName() const { return Name;}
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
	Value* V = NamedValues[Name];
	return V ? V : LogErrorV("Unknown variable name");
}

Value* BinaryExprAST::Codegen()
{
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
	case '>':
	{
		L = Builder->CreateFCmpUGT(L, R, "cmptmp");
		return Builder->CreateUIToFP(L, Type::getDoubleTy(*TheContext), "booltmp");
	}
	default:
		return LogErrorV("Invalid binary operator");
	}
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

	Value *ThenV = Then->Codegen();
	if (!ThenV)
		return nullptr;

	Builder->CreateBr(MergeBB);
	// Codegen of 'Then' can change the current block, update ThenBB for the PHI.
	ThenBB = Builder->GetInsertBlock();

	// Emit else block.
	TheFunction->insert(TheFunction->end(), ElseBB);
	Builder->SetInsertPoint(ElseBB);

	Value *ElseV = Else->Codegen();
	if (!ElseV)
		return nullptr;

	Builder->CreateBr(MergeBB);
	// Codegen of 'Else' can change the current block, update ElseBB for the PHI.
	ElseBB = Builder->GetInsertBlock();

	// Emit merge block.
	TheFunction->insert(TheFunction->end(), MergeBB);
	Builder->SetInsertPoint(MergeBB);
	PHINode *PN = Builder->CreatePHI(Type::getDoubleTy(*TheContext), 2, "iftmp");

	PN->addIncoming(ThenV, ThenBB);
	PN->addIncoming(ElseV, ElseBB);
	return PN;
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
		TheFunction = Proto->Codegen();

	if (!TheFunction)
		return nullptr;

	if (!TheFunction->empty())
		return (Function *)LogErrorV("Function cannot be redefined.");

	// Create a new basic block to start insertion into.
	BasicBlock *BB = BasicBlock::Create(*TheContext, "entry", TheFunction);
	Builder->SetInsertPoint(BB);

	// Record the function arguments in the NamedValues map.
	NamedValues.clear();
	for (auto &Arg : TheFunction->args())
	{
		NamedValues[std::string(Arg.getName())] = &Arg;
	}

	if (Value *RetVal = Body->Codegen())
	{
		// Finish off the function.
		Builder->CreateRet(RetVal);

		// Validate the generated code, checking for consistency.
		verifyFunction(*TheFunction);

		// Optimize the function.
		TheFPM->run(*TheFunction);

		return TheFunction;
	}
	// Error reading body, remove function.
	TheFunction->eraseFromParent();
	return nullptr;
}

#pragma endregion

#endif
