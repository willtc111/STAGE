# STAGE
Simple Text Adventure Game Engine

Team Members: Ari Zerner & Will Carver

## Description
STAGE is a domain-specific language geared towards the development of text-based adventure games.  It allows developers to build a game world which players will be able to interact with through simple textual commands.

## Modules
### DSL Preprocessor
Converts any macros into their proper expansions.
### DSL Compiler and Parser
Provides reasonable feedback on errors in STAGE code.
Feedback includes where in the STAGE code an unexpected symbol was encountered (line and column) and what symbol was found/expected.
Builds the starting state of the game from the STAGE code.
### Game Engine
Manages the state of the world/player during gameplay.
Executes action commands received from the UI to update the state of the world/player.
### User Interface
Provides output of what happens in the game.
Takes input from the player in the form of text commands.
Provides auxiliary commands such as "help" that are not part of the base game.
### Testing
Provides QuickCheck tests for the other modules (currently only for the parser).

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

## Example STAGE Program

The game state is described by
  "You are in "
+ description by its description of the current location
+ description by its description of the player.

The player is described by
  if it does contain something that is a trinket
    then "\nYou are carrying a trinket.",
    but  "" otherwise
and starts in room1.

A trinket is described by "A small trinket.".
Thing trinket is a trinket.

The player can take and drop trinket as "the trinket".

A room is described by
  its name + ". "
+ "There is a door to another room. "
+ if it does contain something that is a trinket
    then "This room contains a trinket.",
    but  "" otherwise.

Location room1 is a room named "Room 1" that contains thing trinket.
Location room2 is a room named "Room 2".

Moving from room1 to room2 is invoked with "go through door" and described by "You move to Room 2".

Moving from room2 to room1 is invoked with "go through door" and described by "You move to Room 1".

The default quit action is available.

## Takeaways from the project
Learning to use Parsec was a large part of this project.  While the execution of the game is relatively straight forward, the compilation from STAGE code into the game was easily the most difficult portion to develop.  Having learned about parsing in class gave us a solid foundation to start with, though.

Learning about pretty printing and QuickCheck testing in class came in handy as well, as the testing of the parser combined both of these topics and presented a challenging twist on the simpler examples we had covered previously.

Designing the STAGE language itself was a continuous process, as we found problems or potential simplifications throughout the project's development.  Keeping track of the growing web of dependencies between modules was an ongoing task, but one we were able to handle very well.

While we weren't able to implement everything we had initially planned, the development of this project was an exciting challenge that really helped to solidify many of the topics we had previously covered in during the semester.
