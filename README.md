# Snake game 1.0

Snake game for DOS written in TASM.

## Requirements:

- tasm.exe
- tlink.exe

## Getting started:

Compile:
```
tasm snake.asm
```

Link:
```
tlink snake.obj /t
```

## How to use:

You need to reach max snake length (15) to win.
Control keys are described at the start.
Top and bottom walls are portals, rigth wall makes you bounce and left one kills you. There are three types of artefacts: increment, decrement or death.

Increment artefact:

![Screenshot](./img/increment.png )

Decrement artefact:

![Screenshot](./img/decrement.png )

Death artefact:

![Screenshot](./img/death.png )

You can chose self-intersection behavior, initial length and amount of death artefacts with command line arguments.

```
snake -h
snake <-l> <-m> <-p>
```

Keys:
- -h - print help message
- -l - start snake length (min 0, max 15); default=0
- -m - intersection mode (0 - forbidden, 1 - allowed, 2 - cut); default=0
- -p - poison count (min 0, max 10); default=1
