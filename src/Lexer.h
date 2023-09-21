// 词法分析器，把用户输入的字符串翻译成一个个Token

#ifndef LEXER_HPP
#define LEXER_HPP

#pragma once

#include <string>
#include <ctype.h>
#include <stdio.h>
#include <map>
#include "AST.h"

/// GetToken返回的结果要么是未知意义的字符[0~255]的ascii码，或者这些满足pattern的token
enum Token {
	tok_eof = -1,
	tok_def = -2,
	tok_extern = -3,
	tok_identifier = -4,
	tok_number = -5,
};

static std::string IdentifierStr;
static double NumVal;
static int CurTok;						// 当前待解析的Token，利用CurTok后，应该主动调用GetNextToken()去缓冲一个新的Token

/// 除了强行面向过程以外，也可以用状态机模式实现
static int GetToken() {
	static int LastChar = ' ';

	// 忽略空格' '，换行符'\n'，制表符'\t'，回车'\r'
	while (isspace(LastChar))
	{
		LastChar = getchar();
	}

	// 识别Identifier和关键字 [a-zA-Z][a-zA-Z0-9]*
	if (isalpha(LastChar))	// [a-zA-Z]
	{
		IdentifierStr = LastChar;
		while(isalnum(LastChar = getchar()))
		{
			IdentifierStr += LastChar;
		}

		if (IdentifierStr == "def")		// 判断是否是关键字
			return Token::tok_def;
		if (IdentifierStr == "extern")
			return Token::tok_extern;
		return tok_identifier;
	}

	// 识别数字 TODO 这里忽略了不少的异常情况，比如两个'.'
	if (isdigit(LastChar) || LastChar == '.')	// [0-9.]+
	{
		std::string NumStr;
		do {
			NumStr += LastChar;
			LastChar = getchar();
		} while(isdigit(LastChar) || LastChar == '.');

		NumVal = std::stod(NumStr);
		return tok_number;
	}

	// 识别注释
	if (LastChar == '#')
	{
		// Comment until end of line.
		do
		{
			LastChar = getchar();
		}
		while (LastChar != EOF && LastChar != '\n' && LastChar != '\r');

		if (LastChar != EOF)	// 吃完了一整行注释，若未EOF，递归调用，返回下一个有意义的token
			return GetToken();
	}

	// 识别EOF
	if (LastChar == EOF)
		return tok_eof;

	// 识别运算符 比如+-*/()，直接返回ascii码
	int ThisChar = LastChar;
	LastChar = getchar();
	return ThisChar;
}

static int GetNextToken()
{
	return CurTok = GetToken();
}

#pragma region 运算符优先级处理

// TODO 是否有可能有运算符二义性？
static std::map<char, int> BinopPrecedence = {
	{'<', 10},
	{'>', 10},
	{'+', 20},
	{'-', 20},
	{'*', 40},
	{'/', 40},
};

// 获取当前Token的优先级，若不是合法运算符则返回-1
static int GetTokPrecedence()
{
	if (!isascii(CurTok))
		return -1;

	int TokPrec = BinopPrecedence[CurTok];
	if (TokPrec <= 0)	// 如果CurTok为空，则TokPrec得到一个默认值0
		return -1;
}

#pragma endregion

#pragma region 解析当前Token

/// primary
///   ::= identifierexpr
///   ::= numberexpr
///   ::= parenexpr
/// 获取表达式统一的入口，返回的为Error或是一个完整的expr
static std::unique_ptr<ExprAST> ParsePrimary()
{
	switch (CurTok)
	{
	default:
		return LogError("unknown token when expecting an expression");
	case tok_identifier:
		return ParseIdentifierExpr();
	case tok_number:
		return ParseNumberExpr();
	case '(':
		return ParseParenExpr();
	}
}

/// expression
///   ::= primary binoprhs
static std::unique_ptr<ExprAST> ParseExpression()
{
	auto LHS = ParsePrimary();	// 确定LHS是一个完整的主表达式
	if (!LHS)
		return nullptr;

	return ParseBinOpRHS(0, std::move(LHS));	// LHS指针指向当前已经解析出的左表达式
}

/// binoprhs
///   ::= ('+' primary)*
/// 递归调用
static std::unique_ptr<ExprAST> ParseBinOpRHS(int ExprPrec, std::unique_ptr<ExprAST> LHS)
{
	// If this is a binop, find its precedence.
	while (1)
	{
		int TokPrec = GetTokPrecedence();	// 判断CurToken的运算符优先级

		// If this is a binop that binds at least as tightly as the current binop,
		// consume it, otherwise we are done.
		if (TokPrec < ExprPrec)		// 比当前运算符优先级更低/无效Token(-1），不处理它，直接返回LHS
			return LHS;

		int BinOp = CurTok;
		GetNextToken();			// 吃掉当前BinOp，刷新CurTok

		auto RHS = ParsePrimary();	// 解析出运算符右边的PrimaryExpr
		if (!RHS)
			return nullptr;

		// 到此，BinOp和RHS都构造完，
		// 接下去有一个分支：
		//	①(LHS BinOp RHS) NextBinOp unparsed...
		//	②LHS BinOp (RHS NextBinOp unparsed...)
		// 所以我们需要判断NextBinOp的优先级再做决定
		int NextPrec = GetTokPrecedence();
		if (TokPrec < NextPrec)		// 分支②
		{
			// 目前需要完整的解析(RHS NextBinOp unparsed...)，发现可以递归调用
			// 同时不需要GetNextToken()，因为NextBinOp还未处理，留在后续迭代中调用
			RHS = ParseBinOpRHS(TokPrec + 1, std::move(RHS));
			if (!RHS)
				return nullptr;
		}
		LHS = std::make_unique<BinaryExprAST>(BinOp, std::move(LHS), std::move(RHS));	// 直接把当前的LHS BinOp RHS构造成一个AST节点作为新的LHS
	}
}

/// numberexpr ::= number
static std::unique_ptr<ExprAST> ParseNumberExpr()
{
	auto Result = std::make_unique<NumberExprAST>(NumVal);
	GetNextToken();	// eat number. 刷新CurTok
	return std::move(Result);
}

/// parenexpr ::= '(' expression ')'
static std::unique_ptr<ExprAST> ParseParenExpr()
{
	GetNextToken(); // eat '('.
	auto V = ParseExpression();
	if (!V)
		return nullptr;

	if (CurTok != ')')
		return LogError("expected ')'");
	GetNextToken(); // eat ')'. 刷新CurTok
	return V;
}

/// identifierexpr
///   ::= identifier
///   ::= identifier '(' expression* ')'
// 调用该函数时，CurTok必须是tok_identifier
/// var / var(a,b)
static std::unique_ptr<ExprAST> ParseIdentifierExpr()
{
	std::string IdName = IdentifierStr;

	GetNextToken(); // eat identifier. CurTok接收新的内容

	// Variable.
	if (CurTok != '(') // Simple variable ref. 紧接的不是'('，则不是函数调用，只能是变量
		return std::make_unique<VariableExprAST>(IdName);

	// CurTok == '(', Call.
	GetNextToken(); // eat '('. CurTok是第一个形参 或 实参
	std::vector<std::unique_ptr<ExprAST>> Args;
	if (CurTok != ')') // 有可能Identifier(void)的情况
	{
		while (1)
		{
			if (auto Arg = ParseExpression())	// 解析一个表达式（迭代调用）
				Args.push_back(Arg);
			else
				return nullptr;

			if (CurTok == ')')	// 判断是否右括号结束调用
				break;

			if (CurTok != ',')	// 若未结束，应接','再接后续参数，与解析prototypeExpr时不同
				return LogError("Expected ')' or ',' in argument list");
			GetNextToken();
		}
	}
	GetNextToken();		// eat ')'. 刷新CurTok
	return std::make_unique<CallExprAST>(IdName, std::move(Args));
}

/// prototypeexpr ::= identifier '(' identifier* ')'
/// 解析函数原型
static std::unique_ptr<PrototypeAST> ParsePrototype()
{
	if (CurTok != tok_identifier)
		return LogErrorP("Expected function name in prototype");

	std::string FnName = IdentifierStr;
	GetNextToken();	// eat identifier.

	if (CurTok != '(')
		return LogErrorP("Expected '(' in prototype");

	std::vector<std::string> ArgNames;
	while (GetNextToken() == tok_identifier)
	{
		ArgNames.push_back(IdentifierStr);
	}
	if (CurTok != ')')
		return LogErrorP("Expected ')' in prototype");

	GetNextToken();	// eat ')'.

	return std::make_unique<PrototypeAST>(FnName, std::move(ArgNames));
}

/////////////////////////////////////////////////

/// definitionexpr ::= 'def' prototypeexpr
/// def foo(x y) expr
/// 未处理expr后续
static std::unique_ptr<FunctionAST> ParseDefinition()
{
	GetNextToken();	// eat 'def'.

	auto Proto = ParsePrototype();		// eat prototype.
	if (Proto == nullptr)
		return nullptr;

	if (auto FnBody = ParseExpression())	// eat expr 函数体也是一个表达式
		return std::make_unique<FunctionAST>(std::move(Proto), std::move(FnBody));
	return nullptr;
}

/// external ::= 'extern' prototype
/// extern函数也是一个Prototype
/// extern foo(x y)
/// 未处理prototype的后续
static std::unique_ptr<PrototypeAST> ParseExtern()
{
	GetNextToken();	// eat 'extern'.
	return ParsePrototype();	// eat prototype
}

/// toplevelexpr ::= expression
static std::unique_ptr<FunctionAST> ParseTopLevelExpr()
{
	if (auto Expr = ParseExpression())	// 把TopLevelExpr视作一个无identifier无参数的函数来执行
	{
		auto Proto = std::make_unique<PrototypeAST>("", std::vector<std::string>());	// -> 一个表达式等价于标识符和参数都为空的匿名函数
		return std::make_unique<FunctionAST>(std::move(Proto), std::move(Expr));
	}
	return nullptr;
}

#pragma endregion

#endif
