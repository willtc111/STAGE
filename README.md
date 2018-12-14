# STAGE
Simple Text Adventure Game Engine

Team Members: Ari Zerner & Will Carver

## Description
STAGE is a domain-specific language geared towards the development of text-based adventure games.  It allows developers to build a game world which players will be able to interact with through simple textual commands.

## Modules
### DSL Compiler
Provides reasonable feedback on errors in STAGE code
Builds the starting state of the game
### Game Engine
Manages the state of the world/player during gameplay
Executes action commands from the UI to update the state of the world/player
### User Interface
Provides output of what happens in the game
Takes input from the player in the form of text commands
Provides auxillary commands such as "help" or "quit" that are not part of the base game


## DSL Grammar
Start = Stage

List(X) := X | X and X | CommaList(X)
CommaList(X) := X, CommaList(X) | X, and X

Either(X, Y) := X | Y

Maybe(X) := Either(X, ε)

An := a | an

Condition := Either(player, location) Pred | Condition or Condition | List(Condition)

Pred := is unconditional | is Maybe(not) Id | does Maybe(not) contain something that Pred | is Maybe(not) An Id | has Stat Cmp Int | Pred or Pred | List(Pred)

Cmp := = | /= | < | <= | > | >=

Mod := doing nothing | setting Stat to Expr | adding Id | removing Id | Mod if it Pred, and otherwise Mod | modifying everything it contains that Pred by Mod | List(Mod)

Expr := Int | Int Op Int | Stat | Id.Stat

Op := + | - | / | * | %

ThingDesc := String | its id | its Stat | ThingDesc if it Pred, and otherwise ThingDesc | ThingDesc if Condition, and otherwise ThingDesc | for each contained thing, SubThingDesc, separated by String | ThingDesc + ThingDesc

SubThingDesc := its description | ThingDesc

ActionDesc := String | ActionDesc if Condition, and otherwise ActionDesc | description of Either(player, location) by ThingDesc | ActionDesc + ActionDesc

ClassDecl := Either("A", "An") Id Maybe(has Either(stat, stats) List(Id = Int) and) is described by ThingDesc.

ThingDecl := "Thing" Id is An Id Maybe(with Either(stat, stats) List(Id = Int) Maybe(that contains List(Id)).

ActionDecl := "Action" String is available when Condition, modifies player by Mod, modifies current location by Mod before setting location to Id, and is described by ActionDesc.

Decl := ClassDecl | ThingDecl | ActionDecl

Decls := Maybe(Decls Decl)

PlayerDecl := "The" player Maybe(has Either(stat, stats) List(Id = Int) and) Maybe(has Either(thing, things) List(Id) and) starts in Id and is described by ThingDesc.

Stage := NonPlayerDecls PlayerDecl NonPlayerDecls
