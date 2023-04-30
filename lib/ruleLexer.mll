{
    open RuleParser

    let make_registry reg =
        let table = Hashtbl.create 10 in
        let find str =
            try Hashtbl.find table str
            with Not_found -> raise (Failure (Printf.sprintf "Unknown identifier %s" str))
        in
        List.iter (fun (str, value) -> Hashtbl.add table str value) reg;
        find

    let registry = make_registry [
        (* Property *)
        "VerticalSymmetry", Property VerticalSymmetry;
        "HorizontalSymmetry", Property HorizontalSymmetry;
        "AxialSymmetry", Property AxialSymmetry;
        "BlueYellowPath", Property BlueYellowPath;
        "Cylindrical", Property Cylindrical;
        (* Navigation *)
        "Meet", Navigation Meet;
        "Start", Navigation (Start true);
        "End", Navigation (End 0);
        "PathHorizontal", Navigation (PathHorizontal true);
        "PathVertical", Navigation (PathVertical true);
        (* Symbol *)
        "Hexagon", Symbol Hexagon;
        "Square", Symbol Square;
        "Star", Symbol Star;
        "Shape", Symbol (Shape (Block, NoRotation));
        "AntiShape", Symbol (AntiShape (Block, NoRotation));
        "Triad", Symbol Triad;
        "Triangle", Symbol (Triangle 0);
        (* Shape *)
        "Block", Shape Block;
        "Bar2", Shape Bar2;
        "Bar3", Shape Bar3;
        "Bar4", Shape Bar4;
        "Corner", Shape Corner;
        "L", Shape L;
        "LMirrored", Shape LMirrored;
        "T", Shape T;
        "SmallT", Shape SmallT;
        "Block4", Shape Block4;
        "Diagonal2", Shape Diagonal2;
        (* Color *)
        "Red", Color Red;
        "Green", Color Green;
        "Blue", Color Blue;
        "White", Color White;
        "Cyan", Color Cyan;
        "Magenta", Color Magenta;
        "Yellow", Color Yellow;
        "Black", Color Black;
        "Orange", Color Orange;
        (* Rotation *)
        "Rotation90", Rotation Rotation90;
        "Rotation180", Rotation Rotation180;
        "Rotation270", Rotation Rotation270;
        "AnyRotation", Rotation AnyRotation;
        (* Tags *)
        "Disabled", Tag false;
        "Cut", Tag false;
    ]
}

let identifier = ['a'-'z' 'A'-'Z'] ['a'-'z' 'A'-'Z' '0'-'9']+
let integer = ['0'-'9']
let whitespace = [' ' '\t']

rule lex = parse
| ',' { Comma }
| ':' { Colon }
| '=' { Equal }
| whitespace { lex lexbuf }
| identifier as id { registry id }
| integer as i { Int (int_of_char i - int_of_char '0') }
| eof { Eos }
| _ as c { Character c }