import Ficha1

type Etapa = (Hora,Hora)
type Viagem = [Etapa]

--1 a)
etapabemconstruida :: Etapa -> Bool
etapabemconstruida (h1, h2) = horaValida h1 && horaValida h2 && h2 `depoisHora` h1

--1 b)
viagembemconstruida :: Viagem -> Bool
viagembemconstruida [] = True
viagembemconstruida [(h1,h2)] = etapabemconstruida (h1,h2)
viagembemconstruida ((h1, h2):(h3, h4):t) = etapabemconstruida (h1, h2) && viagembemconstruida ((h3, h4):t) && h3 `depoisHora` h2

viagembemconstruida' :: Viagem -> Bool
viagembemconstruida' [] = True
viagembemconstruida' ((h1, h2):t) = etapabemconstruida (h1, h2) && viagembemconstruida' t && h3 `depoisHora` h2
    where (h3,h4) = head t

--1 c)
partidachegada :: Viagem -> (Hora,Hora)
partidachegada [] = error "viagem não pode ser vazia"
partidachegada ((h1,h2):t) = (h1, hf2)
    where  (hf1,hf2) = last t

type Poligonal = [Ponto]

--2 a)
comprimento :: Poligonal -> Double
comprimento [] = 0
comprimento [p] = 0
comprimento (p1:p2:t) = dist p1 p2 + comprimento (p2:t)

--2 b)
linhafechada :: Poligonal -> Bool
linhafechada (h:t) = h == l 
    where l = last t 

--2 c)

data Figura = Circulo Ponto Double
            | Retangulo Ponto Ponto
            | Triangulo Ponto Ponto Ponto deriving(Show, Eq)

triangula :: Poligonal -> [Figura]
triangula (p1:p2:p3:t) = Triangulo p1 p2 p3 : triangula (p1:p3:t)
triangula _ = []

--3 a)
data Contacto = Casa Integer
              | Trab Integer
              | Tlm Integer
              | Email String
              deriving Show
type Nome = String
type Agenda = [(Nome, [Contacto])]
agenda_ex = [("Luis", [Tlm 967556911, Email "luiscunha@gmail.com"]),("Pedro", [Tlm 967544321, Email "olaboas@gamil.com"]) ]

acrescEmail :: Nome -> String -> Agenda -> Agenda
acrescEmail nome email [] = [(nome, [Email email])]
acrescEmail nome email ((nome_c, contactos):t)
    |nome == nome_c = ((nome_c, Email email : contactos):t)
    |otherwise = (nome_c, contactos): acrescEmail nome email t 

--3 b)
verEmails :: Nome -> Agenda -> Maybe [String]
verEmails _ [] = Nothing
verEmails nome ((nome_c, contactos):t)
    |nome == nome_c = Just (filtrarEmails contactos)
    |otherwise = verEmails nome t

filtrarEmails :: [Contacto] -> [String]
filtrarEmails [] = []
filtrarEmails (Email e :t) = e : filtrarEmails t 
filtrarEmails (_:t) = filtrarEmails t 

--4 c)

type Dia = Int
type Mes = Int
type Ano = Int

data Data = D Dia Mes Ano
          deriving Show
type TabDN = [(Nome,Data)]

anterior :: Data -> Data -> Bool
anterior (D dia1 mes1 ano1) (D dia2 mes2 ano2) = ano1<ano2 || (ano1 == ano2 && mes1<mes2) ||(ano1==ano2 && mes1==mes2 && dia1<dia2)

--4 d)
ordena :: TabDN -> TabDN
oredena [] = []
ordena (h:t) = inserirDN h (ordena t)

inserirDN :: (Nome,Data) -> TabDN -> TabDN
inserirDN (nome, data') [] = [(nome,data')]
inserirDN (nome,data') ((nomeT,dataT):t) 
    |anterior dataT data' = (nomeT, dataT) : inserirDN (nome,data') t
    |otherwise = (nome,data') : (nomeT,dataT) : t


--pergunta extra : dada uma lista devolve os elementos com indices pares/impares 

indicepares :: [a] -> [a]
pares [] = []
pares [a] = [a]
pares (h:i:t) = h : pares t 

indicepares' :: [a] -> [a]
indicepares' [] = []
indicepares' (h:t) = h : indiceimpares' t

indiceimpares' :: [a] -> [a]
indiceimpares' [] = []
indiceimpares' (h:t) = indicepares' t

indiceimpares :: [a] -> [a]
impares [] = []
impares [a] = []
impares (h:i:t) = i : impares t