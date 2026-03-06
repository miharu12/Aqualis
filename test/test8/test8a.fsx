//#############################################################################
// シーケンス図テスト
let projectname = "test8"
let version = "1.0.0"
//#############################################################################

let outputdir = __SOURCE_DIRECTORY__

#I @"C:\Aqualis\lib\186_0_0_0"
#r "Aqualis.dll"

open Aqualis

Compile [HTMLSequenceDiagram] outputdir 
    "test8a" (version,"aaa") <| fun () ->

    //変数の定義と代入
    ch.I "x" <| fun x ->
    ch.I "y" <| fun y ->
        x <== 0
        y <== x + 1

