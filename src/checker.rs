use crate::{logging::*, Src, lexer::*, parser::*, errors::*};

pub(crate) struct Checker;

impl Checker {
    pub(crate) fn check( src: &Src, logger: &mut CompilationLogger ) -> Result<Vec<Scope>, Vec<SyntaxError>> {
        logger.step( &CHECKING, &src.path );

        let tokens_result = Lexer::tokenize( src );
        logger.substep( &LEXING );

        let tokens = tokens_result?;
        // println!( "{:#?}", lexer );

        let ast_result = Ast::build( &tokens );
        logger.substep( &PARSING );

        logger.substep_done();
        return ast_result;
    }
}
