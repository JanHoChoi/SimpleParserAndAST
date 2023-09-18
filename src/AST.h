//===----------------------------------------------------------------------===//
// Abstract Syntax Tree (aka Parse Tree)
//===----------------------------------------------------------------------===//

#ifndef AST_HPP
#define AST_HPP

#pragma once

#include <string>
#include <vector>

/// ExprAST - Base class for all expression nodes.
class ExprAST
{
public:
	virtual ~ExprAST() {}
};

/// NumberExprAST - Expression class for numeric literals like "1.0".
class NumberExprAST : public ExprAST
{
	double Val;

public:
	NumberExprAST(double val) : Val(val) {}
};

/// VariableExprAST - Expression class for referencing a variable, like "a".
class VariableExprAST : public ExprAST
{
	std::string Name;

public:
	VariableExprAST(const std::string &name) : Name(name) {}
};

/// BinaryExprAST - Expression class for a binary operator. 二元运算符，比如加减乘除
class BinaryExprAST : public ExprAST
{
	char Op;
	ExprAST *LHS, *RHS;

public:
	BinaryExprAST(char op, ExprAST *lhs, ExprAST *rhs)
		: Op(op), LHS(lhs), RHS(rhs) {}
};

/// CallExprAST - Expression class for function calls.
class CallExprAST : public ExprAST
{
	std::string Callee;
	std::vector<ExprAST *> Args;

public:
	CallExprAST(const std::string &callee, std::vector<ExprAST *> &args)
		: Callee(callee), Args(args) {}
};

/// PrototypeAST - This class represents the "prototype" for a function,
/// which captures its name, and its argument names (thus implicitly the number
/// of arguments the function takes).
/// 函数原型/函数接口/函数声明
class PrototypeAST
{
	std::string Name;
	std::vector<std::string> Args;

public:
	PrototypeAST(const std::string &name, const std::vector<std::string> &args)
		: Name(name), Args(args) {}
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
};

#endif