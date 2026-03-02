# GRL Examples
Some examples of Gin Rummy Language.

## Calculate GTO play for the given hand

If a 10-card hand is given, it is assumed to be the draw phase.
If an 11-card hand is given, it is assumed to be the discard phase.

```
# Draw phase
(discard-pile tc 3h ts)  # The discard pile is a list containing at least one card, with the first card being the most recent discard.
(hand 8s 5h ac jd 9c js qc 7h 9d qs)
```

```
# Discard phase
(hand 3d qs 6h 8d 6c jd 5s 9c kd 2h th)
```

## Calculate GTO play for the given hand and play history

```
(discard-pile 2h)  # If discard-pile and history are provided together, the former defines the discard pile *before* history begins.
(hand ks qs 7d ad 9c 2s 5s 2h jc 7h)  # Likewise, when history is provided, this represents the starting hand.
(history
  (i pass)
  (they pass)
  (i draw stock)
  (i discard td)
  (they draw discard)
  (they discard 7h)
  (i draw stock)
  (i discard kd))
```

## Calculate GTO plays for multiple games

Each game has its own scope for hands, discards, templates, etc.

```
(game my-game1
  (discard-pile td)
  (hand ts 8d 2h 4h tc 3c kh 6s 6d 5s))
  
(game my-game2
  (discard-pile 8h)
  (hand 5d 9h jh tc 3s ad 2c ac 7h 6c)
  (history
    ...))
```

## Calculate GTO plays for multiple games using wildcards

You can use as many wildcards as you want, but we warned that this runs in combinatorial time!

```
(hand ac 6h 2c ah 8d ks * 3d 9h 7h)
```

```
(hand ac 6h 2c ah 8d ks *s 3d 9h 7h)
```

```
(hand ac 6h 2c ah 8d ks t* 3d 9h 7h)
```

## Calculate GTO plays for multiple games using templates

Templates substitute one card from each template into the hand per game.
If multiple templates are used, one game will be created for each combination
of cards from all templates. Invalid games (eg involving duplicate cards) will
be skipped automatically.

```
(let template1 ac td qs)
(let template2 6c 3d)
(discard-pile 3h)
(hand template1 template2 qc 8s qd 4c 8h 4s kd 5s)
```

```
(let template ac td qs 6c 2d)
(discard-pile 7d)
(hand template template qc 8s qd 4c 8h 4s kd 5s)
```

## Include scores and other game info

You can specify the current score, score needed to end the game, and the number of points
incurred for a gin, big gin, and undercut. If the current score isn't given, it is asummed
to be 0-0. If these are defined under a game's scope, they apply only to that game. If they
are defined at the top-level scope, they apply to all games.

```
(score 10 15)  # Your score is the first number. Your opponent's score is the second number.
(end-score 200)  # The minimum score needed to end the game.
(gin 20)  # Number of base points a player incurs when their opponent gets a gin.
(big-gin 30)  # Number of base points a player incurs when their opponent gets a big gin.
(undercut 10)  # Number of base points a player incurs when they are undercut.
(knock-threshold 8)  # Maximum hand score permitted for a player to knock
(remaining-stock 31)  # Number of cards left in the stock. This only needs to be given if it can't be inferred from the player's hand, history, and discard pile.
(discard-pile th)
(hand 5h 8d 8c jh kh qh jc 3c 4c 5c)
```
