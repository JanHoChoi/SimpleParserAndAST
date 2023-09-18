// 词法分析器

#ifndef LEXER_HPP
#define LEXER_HPP

#pragma once

#include <string>
#include <ctype.h>
#include <stdio.h>
#include <map>
#include "AST.h"

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
			// if (LastChar )
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

	auto it = BinopPrecedence.find(CurTok);
	if (it == BinopPrecedence.end())
		return -1;
	return BinopPrecedence[CurTok];
}

#pragma endregion

#pragma region 报错辅助

// 区分了三种Error函数，返回值类型不同
ExprAST* Error(const char *Str)
{
	fprintf(stderr, "Error: %s\n", Str);
	return nullptr;
}

PrototypeAST* ErrorP(const char *Str)
{
	Error(Str);
	return nullptr;
}

FunctionAST* ErrorF(const char *Str)
{
	Error(Str);
	return nullptr;
}

#pragma endregion

#pragma region 解析当前Token

static ExprAST* ParseIdentifierExpr();
static ExprAST* ParseNumberExpr();
static ExprAST* ParseParenExpr();
static ExprAST* ParseBinOpRHS(int ExprPrec, ExprAST *LHS);
static PrototypeAST* ParsePrototype();

/// primary
///   ::= identifierexpr
///   ::= numberexpr
///   ::= parenexpr
/// 获取表达式统一的入口，返回的为Error或是一个完整的表达式
static ExprAST* ParsePrimary()
{
	switch (CurTok)
	{
	default:
		return Error("unknown token when expecting an expression");
	case tok_identifier:
		return ParseIdentifierExpr();
	case tok_number:
		return ParseNumberExpr();
	case '(':
		return ParseParenExpr();
	}
}

/// expression
///   ::= primary binoprhs ->
///
static ExprAST* ParseExpression()
{
	ExprAST *LHS = ParsePrimary();	// 解析出第一个主表达式
	if (!LHS)
		return nullptr;

	return ParseBinOpRHS(0, LHS);	// LHS指针指向当前已经解析出的左表达式
}

/// binoprhs
///   ::= ('+' primary)*
static ExprAST* ParseBinOpRHS(int ExprPrec, ExprAST* LHS)
{
	// If this is a binop, find its precedence.
	while (1)
	{
		int TokPrec = GetTokPrecedence();	// 判断CurToken的运算符优先级

		// If this is a binop that binds at least as tightly as the current binop,
		// consume it, otherwise we are done.
		if (TokPrec < ExprPrec)		// 比当前运算符优先级更低/无效Token(-1），先不处理它
			return LHS;

		int BinOp = CurTok;
		GetNextToken();			// 吃掉当前BinOp，刷新CurTok

		ExprAST* RHS = ParsePrimary();
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
			RHS = ParseBinOpRHS(TokPrec + 1, RHS);
			if (!RHS)
				return nullptr;
		}
		LHS = new BinaryExprAST(BinOp, LHS, RHS);	// 直接把当前的（LHS BinOp RHS）构造成一个子节点作为新的LHS
	}
}

/// numberexpr ::= number
static ExprAST* ParseNumberExpr()
{
	ExprAST* Result = new NumberExprAST(NumVal);
	GetNextToken();	// 刷新CurTok
	return Result;
}

/// parenexpr ::= '(' expression ')'
static ExprAST* ParseParenExpr()
{
	GetNextToken(); // eat '('.
	ExprAST *V = ParseExpression();
	if (!V)
		return nullptr;

	if (CurTok != ')')
		return Error("expected ')'");
	GetNextToken(); // eat ')'. 刷新CurTok
	return V;
}

/// identifierexpr
///   ::= identifier
///   ::= identifier '(' expression* ')'
// 调用该函数时，CurTok必须是tok_identifier
static ExprAST* ParseIdentifierExpr()
{
	std::string IdName = IdentifierStr;

	GetNextToken(); // eat identifier. CurTok接收新的内容

	if (CurTok != '(') // Simple variable ref. 紧接的不是'('，则不是函数调用，只能是变量
		return new VariableExprAST(IdName);

	// Call.
	GetNextToken(); // eat '('. CurTok是第一个形参 或 实参
	std::vector<ExprAST *> Args;
	if (CurTok != ')') // 排除Identifier(void)的情况
	{
		while (1)
		{
			ExprAST *Arg = ParseExpression();	// 解析一个表达式
			if (!Arg)
				return nullptr;
			Args.push_back(Arg);

			if (CurTok == ')')	// 判断是否右括号结束调用
				break;

			if (CurTok != ',')	// 若未结束，应接','再接后续参数
				return Error("Expected ')' or ',' in argument list");
			GetNextToken();
		}
	}
	GetNextToken();		// eat ')'. 刷新CurTok
	return new CallExprAST(IdName, Args);
}

/// prototypeexpr ::= identifier '(' identifier* ')'
/// 解析函数原型
static PrototypeAST* ParsePrototype()
{
	if (CurTok != tok_identifier)
		return ErrorP("Expected function name in prototype");

	std::string FnName = IdentifierStr;
	GetNextToken();	// eat identifier.

	if (CurTok != '(')
		return ErrorP("Expected '(' in prototype");

	std::vector<std::string> ArgNames;
	while (GetNextToken() == tok_identifier)
	{
		ArgNames.push_back(IdentifierStr);
	}
	if (CurTok != ')')
		return ErrorP("Expected ')' in prototype");

	GetNextToken();	// eat ')'.

	return new PrototypeAST(FnName, ArgNames);
}

/////////////////////////////////////////////////

/// definitionexpr ::= 'def' prototypeexpr
static FunctionAST* ParseDefinition()
{
	GetNextToken();	// eat 'def'.

	PrototypeAST* Proto = ParsePrototype();
	if (Proto == nullptr)
		return nullptr;

	if (ExprAST* FnBody = ParseExpression())	// 函数体也是一个表达式而已
		return new FunctionAST(Proto, FnBody);
	return nullptr;
}

/// external ::= 'extern' prototype
/// extern函数也是一个Prototype
/// TODO 是否需要处理';'
static PrototypeAST* ParseExtern()
{
	GetNextToken();	// eat 'extern'.
	return ParsePrototype();
}

/// toplevelexpr ::= expression
static FunctionAST* ParseTopLevelExpr()
{
	if (ExprAST* FnBody = ParseExpression())
	{
		PrototypeAST* Proto = new PrototypeAST("", std::vector<std::string>());	// ->标识符和参数都为空的匿名函数
		return new FunctionAST(Proto, FnBody);
	}
	return nullptr;
}

#pragma endregion

#endif
