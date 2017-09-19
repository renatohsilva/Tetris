#lang racket
;; Autores: Renato Henrique Silva RA: 57667
;;          Luiz Paulo Vieira RA: 58472
;; Você deve implementar as funções neste arquivo. Novas funções podem ser
;; criadas, mas todas as funções devem ter testes (no arquivo testes.rkt).
;;
;; Observe que algumas destas funções não tem testes, faz parte do trabalho
;; criar estes testes.
;;
;; Você não precisa se preocupar com ler a tecla pressionada ou desenhar o jogo
;; na tela. O arquivo main.rkt chama uma função que faz isso. Basta você
;; implementar as funções deste arquivo que o jogo funciona.
;;
;; Para ter uma ideia do processo de execução do jogo, execute o arquivo
;; main.rkt sem mudar nada neste arquivo. Uma janela irá aparecer. Pressione
;; algumas teclas e observe a saída no console do DrRacket. Veja o corpo
;; inicial das funções make-tetris-padrao, trata-tecla, trata-tick e desenha.

(require "base.rkt")
(require "tetra-tipos.rkt")
(require 2htdp/image)
(require 2htdp/universe)

(provide make-tetris-padrao
         tetramino->lista-pos
         para-direita
         para-esquerda
         para-baixo
         gira-tetramino
         selecionar-tipo
         soma-posicao
         valida?
         lop-validas?
         lop-livres?
         seleciona-elemento
         fixa
         monta-campo-novo
         insere-parte-tetra
         limpa
         limpa-campo
         linha-cheia?
         acerta-tamanho-campo
         trata-tecla
         trata-tick
         desenha
         livre?
         desce-tetra-automaticamente)

;; -> Tetris
;; Cria o jogo inicial.
;; Esta função é chamada no arquivo main.rkt.

(define (make-tetris-padrao)
  (make-tetris LARGURA-PADRAO ALTURA-PADRAO (stream-tetraminos) TIMEOUT-PADRAO PONTUACAO_INICIAL NIVEL_INICIAL))

;; Tetris, KeyEvent -> Tetris
;; Esta função é chamada quando uma tecla é pressionada.
;; Devolve um jogo com o tetraminó que está caindo movido de acordo com a tecla
;;   "right" - tenta mover para direita
;;   "left"  - tenta mover para esquerda
;;   "up"    - tenta rotacionar
;;   "down"  - tenta mover para baixo
;;    "r"    - renicia jogo
;;    " "    - desce a tecla até fixar  
;;
;; Se a tecla for "right", "left" ou "up" e o movimento não puder ser
;; realizado, o jogo é devolvido sem modificações.
;;
;; Se a tecla for "down" e o movimento não puder ser realizado, tetra é fixado
;; no campo, as linhas completas são removidas, o próximo tetraminó é
;; selecionada para cair e o contador de automovimento retorna ao valor
;; inicial.
;;
;; Se o movimento puder ser realizado, o jogo após o movimento é devolvido.
;;
;; Use a função key=? para comparar o tecla com os valores "right", "left, "up",
;; "down" e "r" para recomeçar o jogo e " " para descer a tecla até fixar.

(define (trata-tecla jogo tecla)
  (cond 
    [(key=? tecla "up") (gira-tetramino jogo)]
    [(key=? tecla "right") (para-direita jogo)]
    [(key=? tecla "down") (para-baixo jogo)]
    [(key=? tecla "left") (para-esquerda jogo)]
    [(key=? tecla " ") (desce-ate-fixar jogo)]
    [(key=? tecla "r") (reinicia jogo)]
    [else jogo]))

;; Tetris -> Tetris 
;;

(define (desce-ate-fixar jogo)
  jogo)

;; Tetris -> Tetris 
;; Reinicia o jogo caso o jogador perca o jogo e aperte r
;; caso contrario devolve o jogo inalterado

(define (reinicia jogo)
  (if (campo-livre? jogo)
      jogo
      (make-tetris-padrao)))


;; Tetris -> Tetramino
;; Troca o tipo da lista na estrutura do tetramino
;; "rotacionando" ele

(define (troca-tipo-lista-tetramino jogo)
  (struct-copy tetramino (tetris-tetra jogo) [rot (remainder (add1 (tetramino-rot (tetris-tetra jogo))) 
                                                             (length (tetramino-tipo (tetris-tetra jogo))))]))

;; Tetris -> Tetramino
;; Move o tetramino para baixo

(define (desce-tetramino jogo)
  (desloca-tetramino (tetris-tetra jogo) (posn (add1 (posn-lin (tetramino-pos (tetris-tetra jogo))))
                                               (posn-col (tetramino-pos (tetris-tetra jogo))))))

;; Tetris, Operação -> Tetramino
;; Move o tetramino para lado direito caso a variavel operacao seja add1, 
;; move o tetramino para lado esquerda caso a variavel operacao seja sub1

(define (desloca-tetramino-horizontal jogo operacao)
  (desloca-tetramino (tetris-tetra jogo) (posn (posn-lin (tetramino-pos (tetris-tetra jogo)))
                                               (operacao (posn-col (tetramino-pos (tetris-tetra jogo)))))))

;; Tetramino, posn -> Tetramino
;; Move o tetramino em posn posições

(define (desloca-tetramino tetra nova-pos)
  (struct-copy tetramino tetra [pos nova-pos]))

;; Tetris, Tetramino -> Tetris
;; Copia o tetramino deslocado para o tetris de entrada

(define (tenta-deslocar-tetramino jogo tetra-movimentado)
  (cond [(lop-livres? (tetramino->lista-pos
                       tetra-movimentado)
                      (tetris-campo jogo)
                      (tetris-largura jogo)
                      (tetris-altura jogo))
         (struct-copy tetris jogo [tetra tetra-movimentado])]
        [else jogo]))

;; Tetris -> Tetris
;; "Rotaciona" o tetramino caso seja possivel, 
;; caso contrario devolve o jogo inalterado

(define (gira-tetramino jogo)
  (tenta-deslocar-tetramino jogo (troca-tipo-lista-tetramino jogo)))

;; Tetris -> Tetris
;; Move o tetramino para direita caso seja possivel, 
;; caso contrario devolve o jogo inalterado

(define (para-direita jogo)
  (tenta-deslocar-tetramino jogo (desloca-tetramino-horizontal jogo add1)))

;; Tetris -> Tetris
;; Move o tetramino para esquerda caso seja possivel, 
;; caso contrario devolve o jogo inalterado

(define (para-esquerda jogo)
  (tenta-deslocar-tetramino jogo (desloca-tetramino-horizontal jogo sub1)))

;; Tetris -> Tetris
;; Move o tetramino para baixo para baixo caso seja possivel, 
;; caso contrario a peça é fixado e é feita uma tentativa de limpar o jogo

(define (para-baixo jogo)
  (cond [(lop-livres? (tetramino->lista-pos
                       (desce-tetramino jogo))
                      (tetris-campo jogo)
                      (tetris-largura jogo)
                      (tetris-altura jogo))
         (struct-copy tetris jogo [tetra (desce-tetramino jogo)])]
        [else (add-proximo-tetra (limpa (fixa jogo)))]))

;; Tetris -> Tetris
;; Função que trata um tick. Esta função é chamada 28 vezes por segundo, ela
;; deve mover o tetra para baixo depois de uma determinada quantidade
;; (TIMEOUT-PADRAO) de ticks. Se o jogador mover para baixo e fixar o
;; tetraminó, a contagem deve reiniciar.

(define (trata-tick jogo)
  (cond[(campo-livre? jogo)
        (desce-tetra-automaticamente jogo)]
       [else jogo]))

;; Timeout, nível -> Timeout
;; diminui o valor do nível do valor do timeout do jogo, 
;; aumentando assim sua velocidade de decida

(define (sobe-dificuldade time nivel)
  (cond [(> (- time nivel) 1)
         (- time nivel)]
        [else 1]))

;; Tetris -> Tetris
;; Desce um novo tetra se o timeout for 0, subtrai 1 do timeout do jogo caso contrario

(define (desce-tetra-automaticamente jogo)
  (cond[(zero? (tetris-timeout jogo))
        (refresh-timeout (para-baixo jogo) 
                         (sobe-dificuldade TIMEOUT-PADRAO
                                           (tetris-nivel jogo)))]
       [else (refresh-timeout jogo 
                              (sub1 (tetris-timeout jogo)))]))

;; Tetris, Numero -> Tetris
;; Atualiza o timeout

(define (refresh-timeout jogo time)
  (struct-copy tetris jogo [timeout time]))

;; Tetris -> Tetris
;; Adiciona um novo tetramino ao jogo

(define (add-proximo-tetra jogo)
  (struct-copy tetris jogo [tetra 
                            (centraliza (stream-first (tetris-proximos jogo)) 
                                        (tetris-largura jogo))]
               [proximos 
                (stream-rest (tetris-proximos jogo))]))


;; Tetris -> boolean
;; Esta função é chamada para verificar se o jogo ainda esta sendo executado

(define (campo-livre? jogo)
  (lop-livres? (tetramino->lista-pos
                (tetris-tetra jogo))
               (tetris-campo jogo)
               (tetris-largura jogo)
               (tetris-altura jogo)))

;; Tetris -> Imagem
;; Esta função é chamada quando o jogo precisa ser desenhado na tela. Devolve
;; uma imagem que representa o jogo.
;; Veja as funções pré-definidas rectangle, beside, above e overlay (entre
;; outras)no pacote 2htdp/image.

(define tela-final
  (overlay (above (text "Você perdeu!" 17 "black")
                  (text  "Aperte r para recomeçar" 15 "black"))
           (rectangle 165 80 "solid" (make-color 255 255 255 255))
           (rectangle 175 90 "solid" (make-color 255 255 255 155))
           (rectangle 185 100 "solid" (make-color 255 255 255 55))
           (rectangle 390 420 "solid" "black")))

;; Tetris -> Imagem
;; Desenha o jogo caso o jogo ainda não tenha acabado, retorna a tela final
;; caso contrario

(define (desenha jogo)
  (define largura (* (tetris-largura jogo)
                     Q-LARGURA))
  (define altura (* (tetris-altura jogo)
                    Q-ALTURA))
  (cond [(campo-livre? jogo)
         (beside/align "top" (frame (desenha-matriz (tetris-campo (fixa jogo))))
                       (above (above (frame (beside (overlay (text "Pontuação: " 14 "black")
                                                             (rectangle 93 25 "solid" COR-FUNDO))
                                                    (overlay (text (number->string
                                                                    (tetris-pontuacao jogo))
                                                                   14 
                                                                   "black")
                                                             (rectangle 93 25 "solid" COR-FUNDO))))
                                     (frame (beside (overlay (text "Nível: " 14 "black")
                                                             (rectangle 93 25 "solid" COR-FUNDO))
                                                    (overlay (text (number->string
                                                                    (tetris-nivel jogo))
                                                                   14 
                                                                   "black")
                                                             (rectangle 93 25 "solid" COR-FUNDO)))))
                              (frame (above (overlay (text "Próximo" 14 "black")
                                                     (rectangle 186 25 "solid" COR-FUNDO))
                                            (overlay (desenha-quadro-proximos (stream-first (tetris-proximos jogo)))
                                                     (rectangle 186 100 "solid" COR-FUNDO))))
                              (frame (overlay (text/font "TETRIS" 19 "black"
                                                         "Gill Sans" 'swiss 'normal 'bold #f)
                                              (rectangle 186 225 "solid" COR-FUNDO)))))]
        [else tela-final]))

;; Matriz, Tetramino -> Imagem
;; Desenha a matriz relacionada ao jogo

(define (desenha-matriz matriz)
  (cond[(empty? matriz) empty-image] 
       [else (above (desenha-linha (first matriz))
                    (desenha-matriz (rest matriz)))]))

;; Linha -> Imagem
;; Desenha a linha relacionada ao jogo

(define (desenha-linha linha)
  (cond[(empty? linha) empty-image] 
       [else (beside (desenha-quadrado (list-ref CORES (first linha)))
                     (desenha-linha (rest linha)))]))

;; Cor -> Imagem
;; Desenha um retangulo com o parametro cor 

(define (desenha-quadrado cor)
  (cond [(not (equal? cor COR-FUNDO))
         (frame (rectangle Q-LARGURA Q-ALTURA "solid" cor))]
        [else (rectangle Q-LARGURA Q-ALTURA "solid" cor)]))

;; Tetramino -> Imagem
;; Desenha um quadro com o proximo tetramino a ser inserido no jogo 

(define (desenha-quadro-proximos tetra)
  (define matriz (selecionar-tipo (tetramino-tipo tetra)
                                  (tetramino-rot tetra)))
  (desenha-matriz-proximos matriz tetra))


;; Matriz, Tetramino -> Imagem
;; Desenha a matriz relacionada ao próximo elemento do jogo jogo

(define (desenha-matriz-proximos matriz tetra)
  (cond[(empty? matriz) empty-image] 
       [else (above (desenha-linha-proximos (first matriz)
                                            (tetramino-cor tetra))
                    (desenha-matriz-proximos (rest matriz)
                                             tetra))]))

;; Linha -> Imagem
;; Desenha a linha relacionada ao próximo elemento do jogo

(define (desenha-linha-proximos linha num-cor)
  (cond[(empty? linha) empty-image] 
       [else (beside (cond [(zero? (first linha))
                            (desenha-quadrado (list-ref CORES 0))]
                           [else (desenha-quadrado (list-ref CORES num-cor))])
                     (desenha-linha-proximos (rest linha) num-cor))]))

;; Tetramino -> Lista(Posn)
;; Devolve a lista de posições que t ocupa no campo considerando a rotação e a
;; posição (translação em relação a origem).
;; 
;; Por exemplo, seja TT1 definido como
;; (define TT1 (tetramino T_TIPOS 1 (posn 1 0) T_COR))
;; este tetraminó está na rotação 1 e na posição (posn 1 0). O elemento na
;; posição 1 de T_TIPOS é T1 que é a seguinte lista de listas (definina em
;; tetra-tipos.rkt)
;;    0 1 2     ; colunas
;;              ; linhas
;; '((0 1 0)    ; 0
;;   (0 1 1)    ; 1
;;   (0 1 0)))  ; 2
;;
;; As as posições ocupadas por T1 são marcadas com 1, ou seja, as posições
;; ocupadas por T1 são (posn 0 1) (posn 1 1) (posn 1 2) e (posn 2 1). Estas São
;; as posições em relação a (posn 0 0), mas o tetraminó está na posição
;; (posn 1 0), desta forma, precisamos fazer a translação das posições. Para
;; isto, somamos o ponto (posn 1 0) a cada ponto de T1, o que resulta em
;; (pos 1 1) (posn 2 1) (posn 2 2) (posn 3 1). Observe que é posível ter
;; um deslocamento em relação a origem negativa. Por exemplo, se a posição de
;; TT1 fosse (posn 0 -1), obteríamos como resposta da função a lista com
;; as posições (posn 0 0) (posn 1 0) (pos 1 1) (pos 2 0).
;;
;; Veja os testes para outros exemplos de como esta função deve funcionar.

(define (tetramino->lista-pos t)
  (define tetra-tipo (selecionar-tipo (tetramino-tipo t)
                                      (tetramino-rot t)))
  (soma-posicao (filter posn? (flatten (percorre-lista tetra-tipo)))
                (tetramino-pos t)))

;; Lista(Numeros) -> Lista(Posn)
;; Percorre a lista buscando valores que não são iguais a zero
;; e monta uma lista de posições com os indices destes valores que não são iguais a zero

(define (percorre-lista tetra-tipo)
  (for/list ([i tetra-tipo]
             [ti (build-list (length tetra-tipo)
                             values)])
    (for/list ([j i]
               [tj (build-list (length i)
                               values)])
      (cond [(and (not (zero? j)))
             (make-posn ti tj)]
            [else empty]))))

;; Linha, Coluna -> Posn
;; Cria uma posicao de acordo com os parametros de linha e de coluna

(define (make-posn linha coluna)
  (posn linha coluna))

;; Lista(Posn), Posn -> Lista(Posn)
;; Devolve uma lista de posições com a posição de posn
;; somado a cada item da lista de entrada

(define (soma-posicao tetra-posicoes posicao)
  (cond [(empty? tetra-posicoes) empty]
        [else (cons (make-posn (+ (posn-lin (first tetra-posicoes))
                                  (posn-lin posicao))  
                               (+ (posn-col (first tetra-posicoes))
                                  (posn-col posicao)))
                    (soma-posicao (rest tetra-posicoes) posicao))]))

;; Tetramino-tipo, Tatramino-rot -> Lista(Números)
;; Devolve uma lista de números referente ao tetramino
;; de acordo o tipo e a rotação do tetramino

(define (selecionar-tipo tipo rot)
  (cond [(empty? tipo) empty]
        [else (list-ref tipo rot)]))

;; Lista(Posn), Natural, Natural -> Boolean
;; Devolve verdadeiro se todas os posições de lp são válidas, isto é, estão
;; dentro de um campo de tamanho largura x altura. Devolve falso caso
;; contrário.

(define (lop-validas? lp largura altura)
  (cond[(empty? lp) true]   
       [else (and (valida? (first lp) largura altura) 
                  (lop-validas? (rest lp) largura altura))]))

;; Posn, Natural, Natural -> Boolean
;; Devolve verdadeiro se z posição de lp é válida, isto é, estão
;; dentro de um campo de tamanho largura x altura. Devolve falso caso
;; contrário.

(define (valida? pos largura altura)
  (and (>= (posn-lin pos) 0)
       (< (posn-lin pos) altura)
       (>= (posn-col pos) 0)
       (< (posn-col pos) largura)))

;; Lista(Posn), Campo -> Boolean
;; Devolve verdadeiro se todas as posição de lp estão livres no campo. Devolve
;; falso caso contrário.
;; Requer que todas as posições em lp sejam válidas.

(define (lop-livres? lp campo largura altura)   
  (cond[(empty? lp) true]         
       [else (and (valida? (first lp)
                           largura altura)
                  (livre? campo (first lp))
                  (lop-livres? (rest lp) 
                               campo
                               largura 
                               altura))]))

;; Campo, Posn -> Boolean
;; Devolve verdadeiro se a posição de pos no campo é 0, false caso contrario

(define (livre? campo pos)
  (zero? (seleciona-elemento campo pos)))

;; Campo, Posn -> Linha
;;Devolve o elemento selecionado do campo,
;;de acordo com a posição do parametro

(define (seleciona-elemento campo pos)
  (list-ref (list-ref campo (posn-lin pos)) 
            (posn-col pos)))

;; Tetris -> Tetris
;; Preenche as posições ocupadas pelo tetraminó (que está caindo) no campo do
;; jogo.
;; Requer que tetraminó não possa ser movido para baixo.

(define (fixa jogo)
  (struct-copy tetris jogo [campo (monta-campo-novo (tetris-campo jogo)
                                                    (tetramino->lista-pos (tetris-tetra jogo))
                                                    (tetramino-cor (tetris-tetra jogo)))]))


;; Campo, LP, Cor -> Campo
;; Devolve o campo novo com a movimentação do tetra caso possivel, 
;; devolve o mesmo campo caso contrario

(define (monta-campo-novo campo lp cor)
  (cond[(empty? lp) campo]
       [else (monta-campo-novo (insere-parte-tetra campo 
                                                   (first lp) 
                                                   cor)
                               (rest lp)
                               cor)]))

;; Campo, Posn, Cor -> Campo
;; Devolve o campo novo com a inserção de uma parte do tetra caso possivel, 
;; devolve o mesmo campo caso contrario

(define (insere-parte-tetra campo p cor)
  (for/list([i campo]
            [ti (build-list (length campo) 
                            values)])
    (for/list([j i]
              [tj (build-list (length i) 
                              values)])
      (cond[(and (= (posn-lin p)
                    ti)
                 (= (posn-col p)
                    tj))
            cor]
           [else j]))))

;; Tetris -> Tetris
;; Devolve um jogo sem as linhas que estão completas, isto é, as linhas que não
;; tem nenhum quadrado vazio. O jogo devolvido tem o mesmo tamanho do jogo de
;; entrada.

(define (limpa jogo)
  (define campo-limpo (limpa-campo (tetris-campo jogo)))
  (define linhas-retiradas (- (tetris-altura jogo)
                              (length campo-limpo)))
  (define nova-pontuacao (muda-pontuacao jogo linhas-retiradas))
  (define campo-limpo-acertado (acerta-tamanho-campo campo-limpo 
                                                     linhas-retiradas
                                                     (tetris-largura jogo)))
  (cond [(empty? jogo) empty]
        [else (struct-copy tetris jogo [campo campo-limpo-acertado]
                           [pontuacao nova-pontuacao]
                           [nivel (muda-nivel jogo nova-pontuacao)])]))

;; Tetris, Número de linhas -> Tetris
;; Atualiza a pontuação do jogo de acordo com o número de linhas que foram retiradas

(define (muda-pontuacao jogo linhas-retiradas)
  (+ (tetris-pontuacao jogo)
     linhas-retiradas))

;; Tetris, Pontuação -> Tetris
;; Atualiza o nivel do jogo de acordo com o pontuação já obtida

(define (muda-nivel jogo nova-pontuacao)
  (cond [(equal? (tetris-pontuacao jogo)
                 nova-pontuacao)
         (tetris-nivel jogo)]
        [else (quotient nova-pontuacao DIVISOR)]))

;; Campo, Número -> Campo
;; Retorna o campo sem as linhas completas e com as linhas vazias adicionadas
;; se necessario nas posições iniciais

(define (limpa-campo campo)
  (cond [(empty? campo) empty]
        [else (cond [(linha-cheia? (first campo))
                     (limpa-campo (rest campo))]
                    [else (cons (first campo)
                                (limpa-campo (rest campo)))])]))

;; Campo -> Campo
;; Adiciona ao campo o número de linhas vazias referentes 
;; ao número de linhas completas que foram retiradas

(define (acerta-tamanho-campo campo-limpo 
                              num-linhas-retiradas 
                              largura)
  (append (make-campo largura num-linhas-retiradas) campo-limpo))

;; Linha -> Booleano
;; Retorna true se a linha esta preenchida e false caso contrario

(define (linha-cheia? linha)
  (empty? (filter zero? linha)))

;; -> Stream(Tetramino)
;; Cria um stream randômico de tetraminós.
;; Esta função não precisa de testes.
;; Você tem que implementar esta função, o corpo incial deve ser descartado.

;(define (stream-tetraminos)
;  (stream-cons T empty-stream))

(define (gera-aleatorio)
  (random 7))

(define (concatena-tetramino-aleatorio-no-stream num-aleatorio)
  (stream-cons (list-ref TETRAMINOS num-aleatorio)
               (concatena-tetramino-aleatorio-no-stream (gera-aleatorio))))

;; Stream -> Stream
;; retorna um stream de tetraminos

(define (stream-tetraminos) 
  (concatena-tetramino-aleatorio-no-stream (gera-aleatorio)))