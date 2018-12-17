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
Provides auxiliary commands such as "help" or "quit" that are not part of the base game


## DSL Grammar
Start = Stage

List(X) := X and X | X, CommaList(X), and X
CommaList(X) := X, CommaList(X) | X

Either(X, Y) := X | Y

Maybe(X) := Either(X, ε)

An := a | an

Stats := Either(stat Id = Int, stats List(Id = Int))

Things := Either(thing Id, things List(Id))

Condition := the Either(player, current location) Pred | either Condition or Condition | both Condition and Condition

Pred := is unconditional | is Maybe(not) thing Id | does Maybe(not) contain something that Pred | is Maybe(not) An Id | has Id Cmp Expr | either Pred or Pred | both Pred and Pred

Cmp := = | /= | < | <= | > | >=

Mod := doing nothing | setting its Id to Expr | giving it Id | taking away everything it contains that Pred | if it Pred then Mod, but Mod otherwise | first Mod and then Mod

Expr := (Expr) | Nat | -Expr | Expr Op Expr | its Id | Id's Id | the player's Id

Op := + | - | * | / | %

ThingDesc := String | its name | the value of its Id | if it Pred then ThingDesc, but ThingDesc otherwise | if Condition then ThingDesc, but ThingDesc otherwise | for each contained thing, SubThingDesc, separated by String | ThingDesc + ThingDesc

SubThingDesc := its description | ThingDesc

ActionDesc := String | if Condition then ActionDesc, but ActionDesc otherwise | description by SubThingDesc of the Either(player, current location) | ActionDesc + ActionDesc

ClassDecl := Either("A", "An") Id Maybe(has Stats and) is described by ThingDesc.

ThingDecl := "Thing" Id is An Id Maybe(named String) Maybe(with Stats) Maybe(that contains Things).

ActionDecl := "Action" String is available when Condition, modifies the player by Mod, modifies the current location by Mod Maybe(before setting the current location to Id), and is described by ActionDesc. | "Action" String is available when Condition, ends the game, and is described by ActionDesc.

Decl := ClassDecl | ThingDecl | ActionDecl

Decls := Maybe(Decls Decl)

PlayerDecl := "The" player Maybe(has Stats and) Maybe(has Things and) Maybe(is described by ThingDesc and) starts in Id.

WorldDescDecl := "The" game state is described by ActionDesc.

Stage := Decls Either(PlayerDecl Decls WorldDescDecl, WorldDescDecl Decls PlayerDecl) Decls

## Macros

"Location" is a synonym for "Thing".

"is always available" can be used instead of "is available when..." when declaring an action that is always available.

"Moving from <place1 :: Id> to <place2 :: Id> is invoked with <name :: String> and described by <desc :: String>." is shorthand for declaring an action to move between places.

"The player can take and drop <thing :: Id> as <name :: String>." is shorthand for declaring a pair of actions to take and drop a thing. The given name will be used in the action name, and need not be the same as the thing's name.

"The default quit action is available." is shorthand for declaring an action to silently quit the game.
