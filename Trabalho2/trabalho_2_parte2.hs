-- Exercício 6

data Exp = Val Float
         | Add Exp Exp
         | Sub Exp Exp
         | Mul Exp Exp
         | Div Exp Exp

avalia:: Exp -> Float
avalia (Val x) = x
avalia (Add exp1 exp2) = (avalia exp1) + (avalia exp2)
avalia (Sub exp1 exp2) = (avalia exp1) - (avalia exp2)
avalia (Mul exp1 exp2) = (avalia exp1) * (avalia exp2)
avalia (Div exp1 exp2) = (avalia exp1) / (avalia exp2)

a1,a2 :: Exp
a1 = Div (Mul (Add (Val 3) (Val 12)) (Sub (Val 15) (Val 5))) (Mul (Val 1) (Val 3))
a2 = Sub (Val 0) (Mul (Add (Sub (Add (Val 6) (Val 8)) (Val 5)) (Val 1)) (Add (Val 2) (Div (Val 6) (Val 2))))



-- Exercício 7

-- A
data Jogada = Pedra | Papel | Tesoura deriving (Eq, Show)

-- B
vence:: Jogada -> Jogada -> Bool
vence j1 j2 | j1 == Pedra && j2 == Tesoura = True
            | j1 == Papel && j2 == Pedra = True
            | j1 == Tesoura && j2 == Papel = True
            | otherwise = False

-- C
vencedoras:: [(Jogada, Jogada)] -> [Jogada]
vencedoras [] = []
vencedoras ((x,y):xs) | x == y = x:vencedoras xs
                      | vence x y = x:vencedoras xs
                      | otherwise = y:vencedoras xs



-- Exercício 8

-- A
type GrauPertinencia = Float
data Nebuloso = Verdadeiro | Falso | Talvez GrauPertinencia deriving (Show)

-- B
fuzzifica:: GrauPertinencia -> Nebuloso
fuzzifica grau | grau <= 0 = Falso
               | grau >= 1 = Verdadeiro
               | otherwise = Talvez grau

-- C
verifica_alto:: Float -> Nebuloso
verifica_alto altura =  fuzzifica ((altura - 1.7) / 0.2)

-- D
verifica_barato:: Float -> Nebuloso
verifica_barato custo = fuzzifica ((50000 - custo) / 20000)



-- Exercício 9

-- A
data Ano = Primeiro | Segundo | Terceiro deriving (Show)
data NomeC = Nacional | Olimpo | Gabarito deriving (Show)
type Matricula = String
type Altura = Float
type Peso = Float
data NomeU = UFU | UNITRI | UNA deriving (Show)
data Curso = Computação | Medicina | Direito | Música deriving (Show)
type Idade = Int


data Estudante = Uni NomeU Curso Matricula Altura Idade
               | Col Ano NomeC Matricula Altura Peso deriving (Show)

u1,u2,u3,u4,u5,u6,u7,u8,u9,u10,c1,c2,c3,c4,c5,c6,c7,c8,c9,c10:: Estudante

u1 = Uni UFU Música "12345678" 1.50 22
u2 = Uni UNITRI Música "32112343" 1.65 20
u3 = Uni UNA Medicina "34576523"1.57 19
u4 = Uni UNITRI Computação "21346743" 1.75 26
u5 = Uni UNA Medicina "12345678" 1.79 21
u6 = Uni UNITRI Direito "123421222" 1.81 22
u7 = Uni UNA Computação "54367556" 1.85 22
u8 = Uni UNA Medicina "32143324" 1.87 25
u9 = Uni UFU Computação "98563424" 1.94 22
u10 = Uni UFU Direito "56834789" 2.00 24

c1 = Col Terceiro Olimpo "aaaaa" 1.91 100.0
c2 = Col Primeiro Olimpo "aaaab" 1.95 95.4
c3 = Col Segundo Nacional "aaaac" 1.84 84.0
c4 = Col Terceiro Gabarito "aaaad" 1.87 76.3
c5 = Col Primeiro Olimpo "aaaae" 1.76 67.2
c6 = Col Segundo Nacional "aaaaf" 1.74 78.3
c7 = Col Segundo Gabarito "aaaag" 1.71 77.7
c8 = Col Terceiro Olimpo "aaaah" 1.49 50.0
c9 = Col Primeiro Gabarito "aaaai" 1.45 53.1
c10 = Col Terceiro Nacional "aaaaj" 1.47 49.0

type Estudantes = [Estudante]
e1 = [u1,u2,u3,u4,u5,u6,u7,u8,u9,u10,c1,c2,c3,c4,c5,c6,c7,c8,c9,c10]


-- B
descobre_alto:: Estudante -> (Matricula, Nebuloso)
descobre_alto (Uni _ _ matricula altura _) = (matricula, verifica_alto altura)
descobre_alto (Col _ _ matricula altura _) = (matricula, verifica_alto altura)

descobre_altos:: Estudantes -> [(Matricula, Nebuloso)]
descobre_altos estudantes = map descobre_alto estudantes



-- Exercício 10

data ArvBinInt = Nulo |
                 No Int ArvBinInt ArvBinInt deriving (Show, Eq)

-- posOrdem:: ArvBinInt -> [Int]
-- posOrdem Nulo = []
-- posOrdem (No x arvEsq arvDir) = posOrdem arvEsq ++ posOrdem arvDir ++ [x]

-- preOrdem:: ArvBinInt -> [Int]
-- preOrdem Nulo = []
-- preOrdem (No x arvEsq arvDir) = [x] ++ preOrdem arvEsq ++ preOrdem arvDir

folhas:: ArvBinInt -> [Int]
folhas Nulo = []
folhas (No x arvEsq arvDir) | arvEsq == Nulo && arvDir == Nulo = [x] ++ folhas arvEsq ++ folhas arvDir
                            | otherwise = folhas arvEsq ++ folhas arvDir


somaNosInternos:: ArvBinInt -> Int
somaNosInternos Nulo = 0
somaNosInternos (No x arvEsq arvDir) | arvEsq /= Nulo || arvDir /= Nulo = somaNosInternos arvEsq + somaNosInternos arvDir + x
                                     | otherwise = somaNosInternos arvEsq + somaNosInternos arvDir

pertence:: ArvBinInt -> Int -> Bool
pertence Nulo _ = False
pertence (No x arvEsq arvDir) y | x == y = True
                                | otherwise = pertence arvEsq y || pertence arvDir y

