# BombBusters

BombBusters is a cooperative multiplayer game by japanese game writer Hisashi Hayashi, released in 2024 by Cocktail games.
The core objective is to work together to diffuse a bomb by working as a team.

This repository is based on the work of Joshua Rebay, who implemented the logic of `BombBusters - Mission 8` and SMT-backed based logic bots in Haskell. After his work, it was possible to play or watch the game in the terminal consisting of exactlly four players. Those players can be any connection of four of human players and/or logic based bots. 

Now, with this repository, you can play mission 8 on the following webpage: [`localhost:3000`](http://localhost:3000). It's also possible to play with someone remote and to play not just with logic bots but also with machine learning bots. Those are based on a Deep Q-Learning Network.



## Content

1. [Short Introduction of Bombbusters](#short-introduction-of-bombbusters)
2. [How to Play](#how-to-play)
    1. [The Turn](#the-turn)
    2. [Equipment](#equipment)
    3. [Winning and Losing](#winning-and-losing)
    4. [Gameplay](#gameplay)
3. [Installation](#installation)
4. [Configuration](#configuration)
5. [Project Layout](#project-layout)
6. [License](#license)



## Short Introduction of Bombbusters

Every player is a bomb-disposal expert holding a rack of face-down wires. Only you can see the values of your own wires; everyone else sees just their colors and positions. Together the team has to cut every blue and yellow wire before the detonator dial runs out, without ever cutting a red one.

- **Blue wires** (values 1-12, four of each): safe to cut once the team has identified a matching pair.
- **Yellow wires** (values 1-11): safe, but their values are hidden behind color markers.
- **Red wires** (values 1-11): cutting one instantly detonates the bomb.

Cutting a wrong wire ticks it down faster. Reach zero and the bomb explodes.

This implementation plays a 4-player game. Any seat can be assigned to a bot at the start of the match.



## How to Play

For a complete understanding of the game rules, please refer to the [official rulebook](https://www.cocktailgames.com/wp-content/uploads/2023/10/BombBusters_rules_EN.pdf).


### The turn

On your turn you pick one of the available actions:

- **Solo Cut** - cut wires from your own rack: all four copies of a blue value if you hold them, two blues of the same value if all other copies of that value have been cut elsewhere, or all remaining yellow wires
- **Dual Cut** - name one of your own wires (color + value) and point at a wire on another player's rack. If they match, both are cut. If not, the detonator advances. Be careful not to hit a red wire! The bomb will explode immediately and the game will end.
- **Remove Reds** - once every wire left on your rack is red, you can safely set them all aside.

Before cutting starts, the game runs a setup pass in which every player, on their first turn, places one **Info Marker** on one of their own wires. This publicly reveals that wire's value and gives the team something to deduce from.


### Equipment

Each piece of equipment is tied to an **unlocking value**. Once the team has cut two blue wires of that value, the equipment unlocks and can be used once by anyone:

- **Super Detector** - name one of your values, point at another player's rack; they reveal every matching wire.
- **Triple Detector** - name one of your values against three chosen slots on another rack.
- **Double Radiator** - name two of your values against one slot.
- **Stabilizer** - the next cut cannot detonate the bomb, no matter what.

Each player also starts with a personal **Double Detector**: name one of your values plus another candidate against a single slot on another rack.


### Winning and losing

- **Win**: every wire has been cut.
- **Lose**: the detonator reaches 0, or any red wire is cut with an action other than **Remove Reds**.


### Gameplay

Go to [`localhost:3000`](http://localhost:3000). Choose if you want to create a new game or if you want to join an already existing game. To create a game, you need to decide about the amount of human players / logic bots and machine learning bots. To join a game, you need to enter the code for the game. Afterwards, you are in the waitingroom until all human players of the game have registered. When all players are in the waitingroom, the actual game starts.

At the start of the game, you see your rack with 12 or 13 wires on it and the back of the other racks. On the top of the page, you can see the detonator showing how many lives are left, the pool-values for the red and yellow wires, which turn it is at the moment and an info button where you can get more information about the rules of the game. While playing, you'll see all cutted wires with their value at their original position. 



## Installation
#TODO: check
You need [GHC](https://www.haskell.org/ghc/) and
[Cabal](https://www.haskell.org/cabal/) (GHC 9.10 / `base` 4.20). The bot uses
[SBV](https://hackage.haskell.org/package/sbv), which in turn requires an SMT
solver - [Z3](https://github.com/Z3Prover/z3) this project uses z3.

```sh
# Arch
sudo pacman -S z3
# Debian / Ubuntu
sudo apt install z3
# macOS
brew install z3
```

Then, from the project root:

```sh
cabal build
cabal run 
```

When the following text is shown in yyour terminal, the webpage is ready to go:

```sh
http://localhost:3000/
Setting phasers to stun... (port 3000) (ctrl-c to quit)
```



## Configuration

A few knobs are exposed via environment variables (see [`src/Config.hs`](./src/Config.hs)):

| Variable | Purpose |
| --- | --- |
| `BB_FAST=1` | Skip UI delays and screen clears (used for benchmarking). |
| `BB_SEED=<int>` | Seed the RNG so the deal, turn order, and color markers are reproducible. |
| `BB_MAX_MODELS` | Cap on models enumerated by the SMT solver (default 2048). |
| `BB_MAX_DET_PROB` | Detonation-probability threshold above which the bot refuses a cut (default 0.02). |
| `BB_STAB_THRESHOLD` | Success-probability cutoff below which the bot spends a Stabilizer (default 0.6). |
| `BB_EPISTEMIC_CONFIDENCE` | Prior on inferences drawn from other players' past moves (default 0.7). |



## Project Layout

- [`app/Main.hs`](./app/Main.hs) - entry point.
- [`src/GameLogic/`](./src/GameLogic/) - core rules, actions, display, and input handling.
- [`src/BotLogic/`](./src/BotLogic/) - knowledge base, SMT constraint solver, strategies, and epistemic inference.
- [`src/BotML/`](./src/BotML/) - #TODO
- [`frontend/`](./frontend/) - #TODO



## License

BSD-3-Clause. See [`LICENSE`](./LICENSE).
