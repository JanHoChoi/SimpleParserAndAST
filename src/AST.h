//===----------------------------------------------------------------------===//
// Abstract Syntax Tree (aka Parse Tree)
//===----------------------------------------------------------------------===//

#ifndef AST_HPP
#define AST_HPP

#pragma once

#include <string>
#include <vector>
#include <cstdio>
#include <map>
#include <memory>

#include "llvm/DerivedTypes.h"
#include "llvm/IRBuilder.h"
#include "llvm/LLVMContext.h"
#include "llvm/Module.h"
#include "llvm/Analysis/Verifier.h"

using namespace llvm;

static Module* TheModule;	// 顶层容器，存放所有函数和全局变量
static IRBuilder<> Builder(getGlobalContext());
static std::map<std::string, Value*> NamedValues;	// 当前作用域的变量与对应的Value

Value* ErrorV(const char *Str) {
	LogError(Str);
	return nullptr;
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
	virtual Value* Codegen()
	{
		return ConstantFP::get(getGlobalContext(), APFloat(Val));
	}
};

/// VariableExprAST - Expression class for referencing a variable, like "a".
class VariableExprAST : public ExprAST
{
	std::string Name;

public:
	VariableExprAST(const std::string &name) : Name(name) {}
	virtual Value* Codegen()
	{
		Value* V = NamedValues[Name];
		return V ? V : ErrorV("Unknown variable name");
	}
};

/// BinaryExprAST - Expression class for a binary operator. 二元运算符，比如加减乘除
class BinaryExprAST : public ExprAST
{
	char Op;
	std::unique_ptr<ExprAST> LHS, RHS;

public:
	BinaryExprAST(char op, std::unique_ptr<ExprAST> lhs, std::unique_ptr<ExprAST> rhs)
		: Op(op), LHS(lhs), RHS(rhs) {}
	virtual Value* Codegen()
	{
		Value* L = LHS->Codegen();
		Value* R = RHS->Codegen();
		if (L == nullptr || R == nullptr)
			return nullptr;

		switch (Op)
		{
			case '+':
				return Builder.CreateFAdd(L, R, "addtmp");
			case '-':
				return Builder.CreateFSub(L, R, "subtmp");
			case '*':
				return Builder.CreateFMul(L, R, "multmp");
			case '/':
				return Builder.CreateFDiv(L, R, "divmp");
			case '<':
			{
				L = Builder.CreateFCmpULT(L, R, "cmptmp");
				return Builder.CreateUIToFP(L, Type::getDoubleTy(getGlobalContext()), "booltmp");
			}
			case '>':
			{
				L = Builder.CreateFCmpULT(L, R, "cmptmp");
				return Builder.CreateUIToFP(L, Type::getDoubleTy(getGlobalContext()), "booltmp");
			}
			default:
				return ErrorV("Invalid binary operator");

		}
		Value* V = NamedValues[Name];
		return V ? V : ErrorV("Unknown variable name");
	}
};

/// CallExprAST - Expression class for function calls.
class CallExprAST : public ExprAST
{
	std::string Callee;
	std::vector<ExprAST *> Args;

public:
	CallExprAST(const std::string &callee, std::vector<ExprAST *> &args)
		: Callee(callee), Args(args) {}
	virtual Value* Codegen()
	{
		Function* CalleeF = TheModule->getFunction(Callee);
		if (CalleeF == nullptr)
			return ErrorV("Unknown function referenced");

		if (CalleeF->arg_size() != Args.size())
			return ErrorV("Incorrect # arguments passed");

		std::vector<Value*> ArgsV;
		for(unsigned i = 0, e = Args.size(); i != e; ++i)
		{
			ArgsV.push_back(Args[i]->Codegen());
			if (ArgsV.back() == nullptr)
				return nullptr;
		}

		return Builder.CreateCall(CalleeF, ArgsV, "calltmp");
	}
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
	PrototypeAST(const std::string &name, const std::vector<std::string> &args)
		: Name(name), Args(args) {}
	Function* Codegen()
	{
		// 参数全是double类型
		std::vector<Type*> Doubles(Args.size(), Type::getDoubleTy(getGlobalCOntext()));

		// 返回值是单个double类型，参数是N个double类型
		FunctionType* FT = FunctionType::get(Type::getDoubleTy(getGlobalContext()), Doubles, false);

		Function* F = Function::Create(FT, Function::ExternalLinkage, Name, TheModule);

		// 检查函数是否已被定义过
		if (F->getName() != Name)
		{
			F->eraseFromParent();
			F = TheModule->getFunction(Name);	// 获取之前已定义过的函数
		}

		// 检查函数是否定义函数体
		if (!F->empty())
		{
			LogError("Redefinition of function");
			return nullptr;
		}

		// 判断之前的函数定义是否参数个数相同
		if (F->arg_size() != Args.size())
		{
			LogError("Redefinition of function with different # args");
			return nullptr;
		}

		unsigned Idx = 0;
		for( Function::arg_iterator AI = F->arg_begin(); Idx != Args.size(); ++AI, ++Idx)
		{
			// 因为简单实现，所以不检查Args[Idx]是否有冲突
			AI->setName(Args[Idx]);
			NamedValues[Args[Idx]] = AI;
		}
	}
};

/// FunctionAST - This class represents a function definition itself.
/// 函数本身，包括函数原型和函数体
class FunctionAST
{
	PrototypeAST *Proto;
	ExprAST *Body;

public:
	FunctionAST(PrototypeAST *proto, ExprAST *body)
		: Proto(proto), Body(body) {}

	Function* Codegen()
	{
		NamedValues.clear();	// ?? 清空之前的变量的影响

		Function *TheFunction = Proto->Codegen();
		if (TheFunction == 0)
			return 0;

		// Create a new basic block to start insertion into.
		BasicBlock *BB = BasicBlock::Create(getGlobalContext(), "entry", TheFunction);
		Builder.SetInsertPoint(BB);

		if (Value *RetVal = Body->Codegen())
		{
			// Finish off the function.
			Builder.CreateRet(RetVal);

			// Validate the generated code, checking for consistency.
			verifyFunction(*TheFunction);

			return TheFunction;
		}
	}
};

#endif
