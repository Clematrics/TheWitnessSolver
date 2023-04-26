%{
    open Definition
    open Property
    open RuleType

    let ruleTable = Hashtbl.create 10
    let _ = List.iter (fun (str, value) -> Hashtbl.add ruleTable str value) [
        ("VerticalSymmetry", VerticalSymmetry);
        ("HorizontalSymmetry", HorizontalSymmetry);
        ("AxialSymmetry", AxialSymmetry);
        ("BlueYellowPath", BlueYellowPath);
        ("Cylindrical", Cylindrical);
    ]

    type identifierType =
    | SymbolType of (symbol * color)
    | NavType of nav

    type qualifier =
    | EndIdentifier of int
    | SymbolColor of color

    let idTable = Hashtbl.create 80
    let colorTable = Hashtbl.create 10

    let _ =
        List.iter
            (fun (str, value) ->
                Hashtbl.add colorTable str value)
            [
                ("Any", Any);
                ("Red", Red);
                ("Green", Green);
                ("Blue", Blue);
                ("White", White);
                ("Cyan", Cyan);
                ("Magenta", Magenta);
                ("Yellow", Yellow);
                ("Black", Black);
            ];
        List.iter
            (fun (str, value) ->
                Hashtbl.add idTable str (SymbolType (value, Any)))
            [
                ("Hexagon", Hexagon);
                ("Square", Square);
                ("Star", Star);
                ("Triad", Triad);
                ("Unit", Shape (Unit));
                ("TwoBarHorizon", Shape (TwoBarHorizontal));
                ("TwoBarVertica", Shape (TwoBarVertical));
                ("TwoBarAny", Shape (TwoBarAny));
                ("ThreeBarHoriz", Shape (ThreeBarHorizontal));
                ("ThreeBarVerti", Shape (ThreeBarVertical));
                ("ThreeBarAny", Shape (ThreeBarAny));
                ("FourBarHorizo", Shape (FourBarHorizontal));
                ("FourBarVertic", Shape (FourBarVertical));
                ("FourBarAny", Shape (FourBarAny));
                ("CornerTL", Shape (CornerTL));
                ("CornerTR", Shape (CornerTR));
                ("CornerBL", Shape (CornerBL));
                ("CornerBR", Shape (CornerBR));
                ("CornerAny", Shape (CornerAny));
                ("LTL", Shape (LTL));
                ("LTR", Shape (LTR));
                ("LBL", Shape (LBL));
                ("LBR", Shape (LBR));
                ("LAny", Shape (LAny));
                ("LReversedTL", Shape (LReversedTL));
                ("LReversedTR", Shape (LReversedTR));
                ("LReversedBL", Shape (LReversedBL));
                ("LReversedBR", Shape (LReversedBR));
                ("LReversedAny", Shape (LReversedAny));
            ];
        List.iter
            (fun (str, value) ->
                Hashtbl.add idTable str (NavType value))
            [
                ("Meet", Meet);
                ("Start", Start);
                ("End", End 0);
                ("PathHorizontal", PathHorizontal);
                ("PathVertical", PathVertical);
                ("CutPathHorizontal", CutPathHorizontal);
                ("CutPathVertical", CutPathVertical);
            ]

    let find_id id =
        try Hashtbl.find idTable id
        with Not_found ->
            raise (Failure (Printf.sprintf "Unknown identifier %s" id))

    let find_color id =
        try Hashtbl.find colorTable id
        with Not_found ->
            raise (Failure (Printf.sprintf "Unknown color %s" id))

    let find_rule id =
        try Hashtbl.find ruleTable id
        with Not_found ->
            raise (Failure (Printf.sprintf "Unknown rule %s" id))

%}

%token Eof
%token <string> Identifier
%token <char> Character
%token <int> Int
%token Equal
%token Comma
%token Colon
%start <Rule.t> parse

%%

let parse :=
| id=Identifier; Eof;
    { Property (find_rule id) }
| c=Character; Equal; id1=identifier; id2=other_identifier?; Eof;
    {
        (*Warning: 6 end-of-stream conflicts were arbitrarily resolved.
File "<standard.mly>", line 109, characters 18-18:
Warning: production option(other_identifier) -> is never reduced.
Warning: in total, 1 production is never reduced.*)
        match id1, id2 with
        | SymbolType s, None -> Assignment (c, None, Some s)
        | NavType n, None -> Assignment (c, Some n, None)
        | SymbolType s, Some (NavType n)
        | NavType n, Some (SymbolType s) -> Assignment (c, Some n, Some s)
        | NavType _, Some (NavType _) -> raise (Failure "Cannot have two navigation elements in the same assignment")
        | SymbolType _, Some (SymbolType _) -> raise (Failure "Cannot have two symbols in the same assignment")
    }

let other_identifier :=
| Comma; ~=identifier; <>

let identifier :=
| id=Identifier; qualifier=qualifier?;
    {
        let id = find_id id in
        match qualifier with
        | Some (EndIdentifier n) -> begin
            (* Number only valid if End navigation element *)
            match id with
            | NavType (End _) -> NavType (End n)
            | _ -> raise (Failure "Cannot specify an identifier for navigation elements other than end bits")
            end
        | Some (SymbolColor c) -> begin
            (* Color only valid if symbol *)
            match id with
            | SymbolType (s, _) -> SymbolType (s, c)
            | NavType _ -> raise (Failure "Cannot specify a color for navigation elements other than end bits")
            end
        | None -> id
    }

let qualifier :=
| Colon; i=Int;
    { EndIdentifier i }
| Colon; id=Identifier;
    { SymbolColor (find_color id) }
