# BombBusters

BombBusters is a cooperative multiplayer game by japanese game writer Hisashi Hayashi, released in 2024 by Cocktail games.
The core objective is to work together to diffuse a bomb by working as a team.

This repository contains a Haskell implementation of the game, written as part of a bachelor thesis. It supports human players and bots that reason about the hidden state using an SMT-backed constraint solver.

## Overview

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

- **Solo Cut** - cut wires from your own rack: all four
  copies of a blue value if you hold them, two blues of the same value if all other copies of that value have been cut elsewhere, or all remaining yellow wires
- **Dual Cut** - name one of your own wires (color + value) and point at a wire
  on another player's rack. If they match, both are cut. If not, the detonator
  advances. Be careful not to hit a red wire! The bomb will explode immediately and the game will end.
- **Remove Reds** - once every wire left on your rack is red, you can safely set them all aside.

Before cutting starts, the game runs a setup pass in which every player, on
their first turn, places one **Info Marker** on one of their own wires. This
publicly reveals that wire's value and gives the team something to deduce from.

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

### Terminal controls

The game runs in the terminal. At the start you're asked which of the four
seats should be played by bots; the rest are yours. During a turn you'll see
your rack, the other players' racks (colors only), the detonator, cut history,
info markers, and available equipment.

- Numbered menus: type the number and press Enter.
- Wire slots are labelled `a`, `b`, `c`, … - type the single letter and press Enter.
- Press Esc then Enter at any input prompt to go back one step.

## Installation

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

## Configuration

A few knobs are exposed via environment variables (see `src/Config.hs`):

| Variable | Purpose |
| --- | --- |
| `BB_FAST=1` | Skip UI delays and screen clears (used for benchmarking). |
| `BB_SEED=<int>` | Seed the RNG so the deal, turn order, and color markers are reproducible. |
| `BB_MAX_MODELS` | Cap on models enumerated by the SMT solver (default 2048). |
| `BB_MAX_DET_PROB` | Detonation-probability threshold above which the bot refuses a cut (default 0.02). |
| `BB_STAB_THRESHOLD` | Success-probability cutoff below which the bot spends a Stabilizer (default 0.6). |
| `BB_EPISTEMIC_CONFIDENCE` | Prior on inferences drawn from other players' past moves (default 0.7). |

## Project Layout

- `app/Main.hs` - entry point.
- `src/GameLogic/` - core rules, actions, display, and input handling.
- `src/BotLogic/` - knowledge base, SMT constraint solver, strategies, and epistemic inference.

## License

BSD-3-Clause. See `LICENSE`.
