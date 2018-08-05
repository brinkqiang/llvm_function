
#include <stdint.h>

#include <llvm/ADT/IntrusiveRefCntPtr.h>
#include <llvm/Support/Host.h>
#include <llvm/Support/raw_ostream.h>

#include <clang/Basic/FileManager.h>
#include <clang/Basic/LangOptions.h>
#include <clang/Basic/TargetInfo.h>
#include <clang/Basic/TargetOptions.h>

#include <clang/Lex/HeaderSearch.h>
#include <clang/Lex/Preprocessor.h>

#include <clang/Frontend/CompilerInstance.h>
#include <clang/Frontend/TextDiagnosticPrinter.h>

#include <clang/Sema/Sema.h>

#include <clang/AST/ASTConsumer.h>
#include <clang/AST/ASTContext.h>

#include <clang/Parse/ParseAST.h>
#include <clang/Parse/Parser.h>

#include <iostream>

class ASTConsumer : public clang::ASTConsumer {
public:
  virtual void HandleTagDeclDefinition(clang::TagDecl *D) override {
    std::cout << "<class name=\"" << D->getNameAsString() << "\">" << std::endl;
    clang::CXXRecordDecl *cxxrec = llvm::dyn_cast<clang::CXXRecordDecl>(D);
    if (cxxrec) {

      for (auto itr = cxxrec->bases_begin(); itr != cxxrec->bases_end();
           ++itr) {
        std::cout << "\t"
                  << "<inherits name=\"" << itr->getType().getAsString()
                  << "\" />" << std::endl;
      }

      for (auto itr = cxxrec->vbases_begin(); itr != cxxrec->vbases_end();
           ++itr) {
        std::cout << "\t"
                  << "<virtual_inherits name=\"" << itr->getType().getAsString()
                  << "\" />" << std::endl;
      }

      for (auto itr = cxxrec->method_begin(); itr != cxxrec->method_end();
           ++itr) {
        std::cout << "\t"
                  << "<method name=\"" << itr->getNameAsString()
                  << "\" return=\"" << itr->getResultType().getAsString()
                  << "\" >" << std::endl;
        for (auto parm_itr = itr->param_begin(); parm_itr != itr->param_end();
             ++parm_itr) {
          clang::VarDecl *vd = *parm_itr;

          std::cout << "\t\t"
                    << "<param name=\"" << vd->getNameAsString() << "\" type=\""
                    << vd->getType().getAsString() << "\">" << std::endl;
        }
        std::cout << "\t"
                  << "</method>" << std::endl;
      }

      for (auto itr = cxxrec->field_begin(); itr != cxxrec->field_end();
           ++itr) {
        std::cout << "\t"
                  << "<field name=\"" << itr->getNameAsString() << "\" type=\""
                  << itr->getType().getAsString() << "\" />" << std::endl;
      }
    }

    std::cout << "</class>" << std::endl;
  }
};

int main(int argc, char **argv) {
  if (argc < 2) {
    std::cerr << "Usage: MetaGen <filename>" << std::endl;
    return 1;
  }
  clang::DiagnosticOptions diagnosticOptions;
  clang::TextDiagnosticPrinter *pTextDiagnosticPrinter =
      new clang::TextDiagnosticPrinter(llvm::outs(), &diagnosticOptions, true);
  llvm::IntrusiveRefCntPtr<clang::DiagnosticIDs> pDiagIDs;

  clang::DiagnosticsEngine *pDiagnosticsEngine = new clang::DiagnosticsEngine(
      pDiagIDs, &diagnosticOptions, pTextDiagnosticPrinter);

  clang::LangOptions languageOptions;
  languageOptions.CPlusPlus = 1;
  languageOptions.CPlusPlus11 = 1;

  clang::FileSystemOptions fileSystemOptions;

  clang::FileManager fileManager(fileSystemOptions);

  clang::SourceManager sourceManager(*pDiagnosticsEngine, fileManager);

  clang::TargetOptions targetOptions;
  targetOptions.Triple = llvm::sys::getDefaultTargetTriple();

  clang::TargetInfo *pTargetInfo =
      clang::TargetInfo::CreateTargetInfo(*pDiagnosticsEngine, &targetOptions);

  llvm::IntrusiveRefCntPtr<clang::HeaderSearchOptions> hso;
  clang::HeaderSearch headerSearch(hso, fileManager, *pDiagnosticsEngine,
                                   languageOptions, pTargetInfo);

  clang::CompilerInstance compInst;

  llvm::IntrusiveRefCntPtr<clang::PreprocessorOptions> pOpts;

  clang::Preprocessor preprocessor(pOpts, *pDiagnosticsEngine, languageOptions,
                                   pTargetInfo, sourceManager, headerSearch,
                                   compInst);

  const clang::FileEntry *pFile = fileManager.getFile(argv[1]);
  if (pFile) {
    sourceManager.createMainFileID(pFile);

    const clang::TargetInfo &targetInfo = *pTargetInfo;

    clang::IdentifierTable identifierTable(languageOptions);
    clang::SelectorTable selectorTable;

    clang::Builtin::Context builtinContext;
    builtinContext.InitializeTarget(targetInfo);
    clang::ASTContext astContext(languageOptions, sourceManager, pTargetInfo,
                                 identifierTable, selectorTable, builtinContext,
                                 0 /* size_reserve*/);
    ASTConsumer astConsumer;

    clang::Sema sema(preprocessor, astContext, astConsumer);

    pTextDiagnosticPrinter->BeginSourceFile(languageOptions, &preprocessor);
    clang::ParseAST(preprocessor, &astConsumer, astContext);
    pTextDiagnosticPrinter->EndSourceFile();
    identifierTable.PrintStats();
  } else
    std::cerr << "File not found" << std::endl;

  return 0;
}