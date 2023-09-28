#include "AST.h"
#include "Lexer.h"
#include "KaleidoscopeJIT.h"

void InitializeModuleAndPassManager();

static void HandleDefinition()
{
	if (auto FnAST = ParseDefinition())
	{
		if (auto *FnIR = FnAST->Codegen())
		{
			fprintf(stderr, "Read function definition:\n");
			FnIR->print(errs());
			fprintf(stderr, "\n");
			ExitOnErr(TheJIT->addModule(llvm::orc::ThreadSafeModule(std::move(TheModule), std::move(TheContext))));
			InitializeModuleAndPassManager();
		}
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
	if (auto ProtoAST = ParseExtern())
	{
		if (auto *FnIR = ProtoAST->Codegen())
		{
			fprintf(stderr, "Read extern:\n");
			FnIR->print(errs());
			fprintf(stderr, "\n");
		}
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
	if (auto FnAST = ParseTopLevelExpr())
	{
		if (auto *FnIR = FnAST->Codegen())
		{
			fprintf(stderr, "Read top-level expression:\n");
			FnIR->print(errs());
			fprintf(stderr, "\n");

			auto RT = TheJIT->getMainJITDylib().createResourceTracker();

			ExitOnErr(TheJIT->addModule(llvm::orc::ThreadSafeModule(std::move(TheModule), std::move(TheContext)), RT));

			InitializeModuleAndPassManager();

			auto ExprSymbol = ExitOnErr(TheJIT->lookup("__anon_expr"));
			assert(ExprSymbol && "Failed to find function ");

			double (*FP)() = ExprSymbol.getAddress().toPtr<double(*)()>();
			fprintf(stderr, "Evaluated to %f\n", FP());

			ExitOnErr(RT->remove());
		}
	}
	else
	{
		// Skip token for error recovery.
		// 不需要额外打印错误，因为ParseTopLevelExpr()内已经打印了
		GetNextToken();
	}
}

void InitializeModuleAndPassManager()
{
	TheContext = std::make_unique<LLVMContext>();

	TheModule = std::make_unique<Module>("my cool jit", *TheContext);
	TheModule->setDataLayout(TheJIT->getDataLayout());

	Builder = std::make_unique<IRBuilder<>>(*TheContext);

	TheFPM = std::make_unique<legacy::FunctionPassManager>(TheModule.get());
	TheFPM->add(llvm::createInstructionCombiningPass());
	TheFPM->add(llvm::createGVNPass());
	TheFPM->add(llvm::createCFGSimplificationPass());
	TheFPM->doInitialization();
}

int main()
{
	llvm::InitializeNativeTarget();
	llvm::InitializeNativeTargetAsmPrinter();;
	llvm::InitializeNativeTargetAsmParser();

	fprintf(stderr, "ready> ");
	GetNextToken();

	TheJIT = ExitOnErr(llvm::orc::KaleidoscopeJIT::Create());

	InitializeModuleAndPassManager();

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
