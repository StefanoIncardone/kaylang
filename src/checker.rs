use crate::{logging::*, Src, lexer::*, parser::*, errors::*};

pub(crate) struct Checker;

impl<'ast, 'src: 'ast> Checker {
    pub(crate) fn check( src: &'src Src, logger: &mut CompilationLogger ) -> Result<Vec<Scope<'ast>>, Vec<SyntaxError>> {
        logger.step( &CHECKING, &src.path );

        let tokens_result = Lexer::tokenize( src );
        logger.substep( &LEXING );
        let tokens = tokens_result?;

        let ast_result = Ast::build( &tokens );
        logger.substep( &PARSING );
        let ast = ast_result?;

        logger.substep_done();
        return Ok( ast );
    }
}
