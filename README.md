# haskell-label-propagation
MC346 Unicamp - Projeto 1 (Haskell) :: http://www.ic.unicamp.br/~wainer/cursos/2s2019/346/proj-haskell.html

[...]  Alguns dados recebem um label (uma classe) e varios dados não. O objetivo é atribuir uma classe/label para cada um dos dados sem classe.
Faremos esta atribuição por proximidade - se um dado X sem label esta mais proximo de um dado do label A que de qualquer outro dado com label, ele assume o label A. Mas agora X é um dado com label e ele vai propagar seu label para o dado mais proximo sem label (que nao esta ainda mais proximo de outro dado com outro label). [...]

Input Model:
```
aa x1 x2 x3
b3   y1  y2 y3
ez34 z1 z2 z3
...
g78 n1 n2 n3

aa 1
gg7 3
...
```
onde aa (b3 etc.) são os "nomes" dos pontos, e x1 x2 x3 nesse caso são as coordenadas num espaço de 3 dimensões do ponto aa, y1 y2 y3 são as coordenadas do ponto b3 e assim por diante. Voce não sabe de antemão o numero de dimensoes dos dados. Os dados acima estão em 3D mas seu program pode receber dados em N dimensões (não declaradas previamente). As coordenadas do ponto (quantas forem) serão os números que seguem o nome do ponto. Pode haver mais de um branco separando os dados em cada linha.
