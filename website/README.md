# Landing page do ObservaBio

Página única, em Quarto, com o corpo escrito diretamente em HTML dentro de
`index.qmd`. O Quarto é só o motor de build e o compilador do SCSS — não há
navbar, sidebar, busca nem sistema de páginas.

## Rodar local

```sh
cd website
quarto preview      # servidor com recarga automática
quarto render       # gera _site/
```

Depois de `quarto render`, `_site/index.html` abre direto do disco: todos os
caminhos são relativos e não há dependência de servidor. As fontes (Noto Serif,
Inter, IBM Plex Mono) vêm do Google Fonts; sem rede, a página cai nas fontes de
sistema declaradas em `theme.scss` sem quebrar o layout.

## Pixel art

Os sprites não são imagens desenhadas à mão: são gerados por
`scripts/build_pixels.py`, que é a fonte da verdade. O script define cada bicho
como faixas numa grade de caracteres e emite SVG onde cada pixel é um `<rect>`.

```sh
python3 website/scripts/build_pixels.py    # rodar da raiz do repositório
```

Ele escreve:

| Saída | O quê |
| --- | --- |
| `assets/img/favicon.svg` | rosto do lobo-guará em 16×16, com hex literal |
| `index.qmd` | injeta o diagrama de buffer entre os marcadores `<!-- pixel:buffer:start -->` |

A injeção existe por um motivo: SVG referenciado por `<img>` não enxerga as
custom properties da página. Inline, cada `<rect>` usa `fill="var(--pel-*)"`, e
esses tokens apontam para a paleta do app em `theme.scss`. O diagrama de buffer
**é** o design system — mudou o token, mudou a legenda do mapa.

Para editar os pixels, mexa nas faixas em `build_pixels.py` e rode o script de
novo. Não edite o SVG dentro do `index.qmd` na mão: a próxima execução
sobrescreve.

### Os dois ícones de espécie

`assets/img/lobo-guara.png` e `assets/img/javali.png` são a exceção: não saem do
`build_pixels.py`. São arte externa em pixel art, recortada do xadrez de fundo
que vinha chapado no JPG original e reduzida para 64 px de lado maior. Entram
como `<img class="cell-icon">` logo depois do nome comum no cartão
**nome da espécie**, e o `image-rendering: pixelated` do `.cell-icon` é o que
mantém o bloco quadrado em vez de borrar. Trocar a arte é trocar o PNG: não há
script para regerar.

## A esteira animada — que é o herói

O herói **é** a esteira. Não há ilustração na dobra: o que ocupa a primeira tela
é o diagrama de fluxo — planilha → ObservaBio → dados validados — e ele não é
uma ilustração: as quatro linhas são campos reais de uma planilha e o que sai de
cada uma é o que o app devolve de fato.

O movimento é um `<canvas>` (`flow()` em `assets/js/pixel.js`), sem biblioteca.
Quatro correntes de pixels **entram pela borda da tela**, atravessam o cartão do
seu campo, **convergem num ponto só** na aresta do núcleo, voltam a se abrir em
leque do outro lado e saem pela borda oposta — é o gesto do app, e é o que faz o
desenho ler como fluxo em vez de cronômetro.

- **O canvas não vive dentro da esteira**, e sim em `.hero-flow`, dentro da
  faixa full-bleed `.pipe-bleed`. Os cartões param na coluna de texto
  (`.wrap`, 1120px), mas as correntes precisam alcançar as bordas da tela: é
  esse trecho vindo de fora do papel que faz o herói ler como fluxo em vez de
  diagrama numa caixa. A faixa é do tamanho da esteira, não do herói inteiro —
  limpar um canvas de tela cheia a cada quadro se pagaria por nada.
- **Cada corrente tem quatro trechos** (`FEED`, `IN`, `OUT`, `AWAY`), e a
  duração de cada um vem do comprimento que ele de fato percorre, a uma
  velocidade só (`SPEED`, px/ms). Cronometrar por trecho em vez de por
  distância faria o pulo curto numa tela estreita se arrastar enquanto a
  travessia longa numa tela larga dispara — as correntes parariam de ler como
  uma esteira só.
- **O pixel atravessa o cartão.** Os trechos param nas arestas das células,
  nunca por baixo delas: o cartão é opaco e fica acima do canvas
  (`.pipe` é `z-index: 1`), então o pixel é engolido de um lado e reaparece do
  outro. O mesmo objeto continua — trocar de trecho não cria pixel novo.

- **Nada é inventado do lado direito.** Cada pixel que sai é um que entrou: ao
  chegar no núcleo ele vai para uma fila (`held`) e só é reemitido depois do
  tempo de processamento, já na cor do veredicto.
- **Toda coordenada é medida das próprias células**, nunca fixa no código — a
  curva acompanha o layout em qualquer largura, e um `ResizeObserver` remede
  quando as fontes chegam atrasadas e mudam a altura dos cards. Ele observa dois
  boxes: a faixa, que decide onde ficam as bordas da tela, e a grade, que decide
  onde ficam os cartões dentro dela.
- **Toda cor é token**: `--secondary` na entrada e `--success` na saída, lidos
  do `:root` em execução. A cor do alerta não é escolhida no JS — ela é lida do
  próprio chip que a linha vai mostrar, então o pixel sempre pousa na cor do
  badge que está entregando: âmbar no `distributionFlag`, **vermelho**
  (`--error`) na invasora. São dois eixos, não dois volumes — a mesma decisão
  de `inst/app/www/css/05-badges.css`.
- **A esteira alterna entre dois registros** da mesma planilha (`.frame-a` /
  `.frame-b`): o lobo-guará, nativo, ameaçado e declarado num estado onde não
  ocorre; e o javali, que não consta da lista do MMA mas consta das três listas
  de invasoras. Cada registro dispara um tipo diferente de alerta.
- **Qual linha alerta em qual registro é declarado no HTML**, em
  `data-alert="a" | "b"`, não no JavaScript — mexer nos exemplos não exige
  mexer no desenho. O pixel é pintado pela cor do quadro que estará visível
  quando ele *chegar*, e a fase vem de `getAnimations()` da própria animação
  CSS: contar num relógio próprio dessincronizaria, porque o nosso só começa
  quando a esteira entra em tela.
- **Fora de tela o loop para** (`IntersectionObserver`), e um `dt` limitado a
  50 ms impede que uma aba em segundo plano volte adiantada.
- **O herói não entra com `reveal()`.** O que está na dobra tem que estar lá no
  primeiro quadro, e um `transform` em `.pipe` deslocaria os cartões em relação
  ao canvas, que os mede a partir do box da faixa.
- **A animação é descrita em palavras** num `<p class="sr-only">` ao fim do
  herói, porque o gesto está num canvas `aria-hidden`. O conteúdo dos cartões,
  esse, é texto de verdade no DOM e é lido normalmente.

Há três caminhos sem canvas, e todos continuam legíveis: sem JS, abaixo de 820px
e em `prefers-reduced-motion` valem os fios em CSS (`.wire` / `.spark`), que a
classe `.has-flow` esconde quando o canvas assume. No desktop `.pipe-row` é
`display: contents` e quem posiciona é a grade de cinco colunas (`--pipe-cols`,
compartilhada com a faixa de rótulos, senão as duas desalinham); abaixo de 820px
a esteira vira pilha, o fio de saída some e o de entrada gira para a vertical.

## O lobo-guará

Não há mais arte do lobo na página: o retrato `assets/img/lobo-guara.png` e a
prancha `.plate` do herói foram removidos quando a esteira assumiu a dobra.

O que ficou é o lobo como **dado**, não como marca: `Chrysocyon brachyurus` está
na base embarcada (`inst/extdata/`) como **VU** pela Portaria MMA 1.704/2026, e
ocorre em 17 estados — o Amazonas não é um deles. É o registro `.frame-a` da
esteira e o exemplo da seção de verificação geográfica, verificável na base e
não inventado.

O favicon (`assets/img/favicon.svg`) ainda é o rosto do lobo em 16×16 desenhado
pelo `build_pixels.py` — ver Pendências.

## Pendências

- **URL do app.** O botão principal do herói é um `<button disabled>` enquanto o
  app não tem endereço. Para ligá-lo, procure o marcador `CTA-APP` no
  `index.qmd`, troque aquela linha por
  `<a class="btn btn-primary" href="https://SUA-URL">Abrir o ObservaBio</a>`,
  apague o `<p class="hero-note">` logo abaixo e rode `quarto render`. O
  comentário acima do marcador repete a instrução no próprio arquivo.
- **Hospedagem.** Não há `site-url` no `_quarto.yml`, de propósito: sem ele tudo
  fica relativo e o `_site/` abre local. Ao publicar no GitHub Pages, acrescentar
  `site-url` e `repo-url` em `_quarto.yml`.
- **Favicon.** Com a arte do lobo fora da página, a aba do navegador é a única
  coisa que ainda mostra um lobo-guará. Decidir se ele fica como assinatura da
  marca ou se dá lugar a uma marca ligada à esteira; trocar significa redesenhar
  as faixas em `build_pixels.py` e rodar o script.

## Design system

A paleta e a tipografia em `theme.scss` são espelho de
`inst/app/www/css/00-tokens.css` (documentado em `docs/design.md`). Ao mexer em
cor no app, conferir aqui também — são dois arquivos, uma decisão só.
