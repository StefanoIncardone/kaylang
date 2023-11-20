use crate::{logging::*, Src, lexer::*, parser::*, errors::*};

pub(crate) struct Checker;

impl Checker {
    pub(crate) fn check( src: Src, logger: &mut CompilationLogger ) -> Result<AST, SyntaxErrors> {
        logger.step( &CHECKING, &src.path );

        let lexer_result = Lexer::try_from( src );
        logger.substep( &LEXING );

        let mut lexer = lexer_result?;
        // println!( "{:#?}", lexer );

        let ast_result = AST::try_from( &mut lexer );
        logger.substep( &PARSING );

        logger.substep_done();
        return ast_result;
    }
}
