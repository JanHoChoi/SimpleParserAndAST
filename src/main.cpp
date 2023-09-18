#include "AST.h"
#include "Lexer.h"

static void HandleDefinition()
{
	if (ParseDefinition())
	{
		fprintf(stderr, "Parsed a function definition.\n");
	}
	else
	{
		// Skip token for error recovery.
		// 不需要额外打印错误，因为ParseDefinition()内已经打印了
		GetNextToken();
	}
}

static void HandleExtern()
{
	if (ParseExtern())
	{
		fprintf(stderr, "Parsed an extern\n");
	}
	else
	{
		// Skip token for error recovery.
		// 不需要额外打印错误，因为ParseExtern()内已经打印了
		GetNextToken();
	}
}

static void HandleTopLevelExpression()
{
	// Evaluate a top-level expression into an anonymous function.
	if (ParseTopLevelExpr())
	{
		fprintf(stderr, "Parsed a top-level expr\n");
	}
	else
	{
		// Skip token for error recovery.
		// 不需要额外打印错误，因为ParseTopLevelExpr()内已经打印了
		GetNextToken();
	}
}

int main()
{
	fprintf(stderr, "ready> ");
	GetNextToken();

	while (1)
	{
		fprintf(stderr, "ready> ");
		switch (CurTok)
		{
			case tok_eof:
				return 0;
			case ';':
			{
				GetNextToken();		// eat ';' 忽略了分号??
				break;
			}
			case tok_def:
			{
				HandleDefinition();
				break;
			}
			case tok_extern:
			{
				HandleExtern();
				break;
			}
			default:
			{
				HandleTopLevelExpression();
				break;
			}
		}
	}
}
