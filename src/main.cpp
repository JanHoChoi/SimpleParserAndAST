#include "AST.h"
#include "Lexer.h"
#include "KaleidoscopeJIT.h"

#include "llvm/MC/TargetRegistry.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/TargetSelect.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Target/TargetMachine.h"
#include "llvm/Target/TargetOptions.h"
#include "llvm/TargetParser/Host.h"

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
#if USE_JIT
			ExitOnErr(TheJIT->addModule(llvm::orc::ThreadSafeModule(std::move(TheModule), std::move(TheContext))));
			InitializeModuleAndPassManager();
#endif
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
			FunctionProtos[ProtoAST->GetName()] = std::move(ProtoAST);
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
#if USE_JIT
		if (FnAST->Codegen())
		{
			auto RT = TheJIT->getMainJITDylib().createResourceTracker();

			ExitOnErr(TheJIT->addModule(llvm::orc::ThreadSafeModule(std::move(TheModule), std::move(TheContext)), RT));

			InitializeModuleAndPassManager();
			auto ExprSymbol = ExitOnErr(TheJIT->lookup("__anon_expr"));
			assert(ExprSymbol && "Function not found");

			double (*FP)() = ExprSymbol.getAddress().toPtr<double(*)()>();
			fprintf(stderr, "Evaluated to %f\n", FP());

			ExitOnErr(RT->remove());
		}
#else
		FnAST->Codegen();
#endif
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
#if USE_JIT
	TheModule->setDataLayout(TheJIT->getDataLayout());
#endif
	Builder = std::make_unique<IRBuilder<>>(*TheContext);

#if USE_JIT
	TheFPM = std::make_unique<legacy::FunctionPassManager>(TheModule.get());
	TheFPM->add(llvm::createInstructionCombiningPass());
	TheFPM->add(llvm::createGVNPass());
	TheFPM->add(llvm::createCFGSimplificationPass());
	// Promote allocas to registers.
	TheFPM->add(llvm::createPromoteMemoryToRegisterPass());
	// Do simple "peephole" optimizations and bit-twiddling optzns.
	TheFPM->add(createInstructionCombiningPass());
	// Reassociate expressions.
	TheFPM->add(createReassociatePass());
	TheFPM->doInitialization();
#endif
}

#ifdef _WIN32
#define DLLEXPORT __declspec(dllexport)
#else
#define DLLEXPORT
#endif

/// putchard - putchar that takes a double and returns 0.
extern "C" DLLEXPORT double putchard(double X)
{
	fputc((char)X, stderr);
	return 0;
}

/// printd - printf that takes a double prints it as "%f\n", returning 0.
extern "C" DLLEXPORT double printd(double X)
{
	fprintf(stderr, "%f\n", X);
	return 0;
}

int main()
{
#if USE_JIT
	llvm::InitializeNativeTarget();
	llvm::InitializeNativeTargetAsmPrinter();;
	llvm::InitializeNativeTargetAsmParser();
#endif

	fprintf(stderr, "ready> ");
	GetNextToken();

#if USE_JIT
	TheJIT = ExitOnErr(llvm::orc::KaleidoscopeJIT::Create());
#endif

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
			case tok_exit:
			{
				break;
			}
			default:
			{
				HandleTopLevelExpression();
				break;
			}
		}
		if (CurTok == tok_exit)
			break;
	}

	// Initialize the target registry etc.
	InitializeAllTargetInfos();
	InitializeAllTargets();
	InitializeAllTargetMCs();
	InitializeAllAsmParsers();
	InitializeAllAsmPrinters();

	auto TargetTriple = llvm::sys::getDefaultTargetTriple();
	TheModule->setTargetTriple(TargetTriple);

	std::string Error;
	auto Target = TargetRegistry::lookupTarget(TargetTriple, Error);

	// Print an error and exit if we couldn't find the requested target.
	// This generally occurs if we've forgotten to initialise the
	// TargetRegistry or we have a bogus target triple.
	if (!Target)
	{
		errs() << Error;
		return 1;
	}

	auto CPU = "generic";
	auto Features = "";

	TargetOptions opt;
	auto RM = std::optional<Reloc::Model>();
	auto TheTargetMachine = Target->createTargetMachine(TargetTriple, CPU, Features, opt, RM);

	TheModule->setDataLayout(TheTargetMachine->createDataLayout());

	auto Filename = "output.o";
	std::error_code EC;
	raw_fd_ostream dest(Filename, EC, sys::fs::OF_None);

	if (EC)
	{
		errs() << "Could not open file: " << EC.message();
		return 1;
	}

	legacy::PassManager pass;
	auto FileType = CodeGenFileType::CGFT_ObjectFile;

	if (TheTargetMachine->addPassesToEmitFile(pass, dest, nullptr, FileType))
	{
		errs() << "TheTargetMachine can't emit a file of this type";
		return 1;
	}

	pass.run(*TheModule);
	dest.flush();

	outs() << "Wrote " << Filename << "\n";

	return 0;
}
