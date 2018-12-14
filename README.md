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

List(X) := X and X | X, CommaList(X), and X
CommaList(X) := X, CommaList(X) | X

Either(X, Y) := X | Y

Maybe(X) := Either(X, ε)

An := a | an

Stats := Either(stat Id = Int, stats List(Id = Int))

Condition := (Condition) | the Either(player, current location) Pred | Condition or Condition | List(Condition)

Pred := (Pred) | is unconditional | is Maybe(not) Id | does Maybe(not) contain something that Pred | is Maybe(not) An Id | has Id Cmp Expr | Pred or Pred | List(Pred)

Cmp := = | /= | < | <= | > | >=

Mod := doing nothing | setting its Id to Expr | giving it Id | taking Id from it | if it Pred then Mod, but Mod otherwise | modifying by Mod everything it contains | List(Mod)

Expr := (Expr) | Int | Int Op Int | Id | Id.Id | player.Id

Op := + | - | / | * | %

ThingDesc := String | its name | its Id | if it Pred then ThingDesc, but ThingDesc otherwise | if Condition then ThingDesc, but ThingDesc otherwise | for each contained thing, SubThingDesc, separated by String | ThingDesc + ThingDesc

SubThingDesc := its description | ThingDesc

ActionDesc := String | if Condition then ActionDesc, but ActionDesc otherwise | description by ThingDesc of the Either(player, current location) | ActionDesc + ActionDesc

ClassDecl := Either("A", "An") Id Maybe(has Stats and) is described by ThingDesc.

ThingDecl := Either("Thing", "Location") Id is An Id named String Maybe(with Stats) Maybe(that contains Either(Id, List(Id))).

ActionDecl := "Action" String is available when Condition, modifies player by Mod, modifies current location by Mod before setting location to Id, and is described by ActionDesc. | "Action" String is available when Condition, ends the game, and is described by ActionDesc.

Decl := ClassDecl | ThingDecl | ActionDecl

Decls := Maybe(Decls Decl)

PlayerDecl := "The" player Maybe(has Stats and) Maybe(has Either(thing Id, things List(Id)) and) starts in Id and is described by ThingDesc.

Stage := Decls PlayerDecl Decls
