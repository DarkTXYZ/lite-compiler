open OUnit2
open CpecmuCompiler.A1_lexing
open CpecmuCompiler.Token

let tests =
  "sample test suite for lexing"
  >::: [
         ("" >:: fun _ -> assert_equal [ EOF ] (lex ""));
         ("+" >:: fun _ -> assert_equal [ Symbol "+"; EOF ] (lex "+"));
         ( "+ +" >:: fun _ ->
           assert_equal [ Symbol "+"; Symbol "+"; EOF ] (lex "+ +") );
         (* the following tests fail initially *)
         ("++" >:: fun _ -> assert_equal [ Symbol "++"; EOF ] (lex "++"));
         ("'\\n'" >:: fun _ -> assert_equal [ CharLit '\n'; EOF ] (lex "'\\n'"));
         ( "\"hello\"" >:: fun _ ->
           assert_equal [ StringLit "hello"; EOF ] (lex "\"hello\"") );
       ]

let _ = run_test_tt_main tests
