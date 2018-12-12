%token <int> NUMBER
%token HASH
%token AT
%token COMMA
%token COLON
%token TIMES
%token EOF

%start <(int * int * int * int * int) list> claims

%%

claims:
| c = claim*; EOF { c }

claim:
| HASH; id = NUMBER; AT; x = NUMBER; COMMA; y = NUMBER; COLON; w = NUMBER; TIMES; h = NUMBER; { (id, x, y, w, h) }
