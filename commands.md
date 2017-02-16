# Commands

Following are the available commands:

- `RegisterPlayerCommand`
- `MovePlayerCommand`
- `PlayerShootCommand`
- `StartGameCommand`
- `ConfigurePlayerCommand`

OTHER

####ShootingInfo
Struct that describes the shooting capabilities of the player. Fields

- tokens (integer) shooting tokens the player has
- cost (integer) the cost of each shot in terms of tokens


## RegisterPlayerCommand

- payload
```
{"type": "RegisterPlayerCommand"}
```
- when can be sent
At any time during the game. But depending on the value of the `players_can_join_started_game` configuration variable they might
not be able to participate on an ongoin game

- expects response
Yes
  - which response
    If the registration was successful:
    ```
    {
      "type": "RegisterPlayerAck",
        "data": {
          "id": // (string) Player ID,
          "x": // (integer) X coordinate of the player in the arena
          "y": // (integer) Y coordinate of the player in the arena
          "life": // (integer) Life units left on the player
          "shooting": // (ShootingInfo) Details about the shooting capabilities of the player
        }
    }
    ```
    If the player can also join an ongoin game then he will also receive:
    ```
    {
      "type": "StartGameOrder"
    }
    ```
    meaning that he should start playing

    If the registration failed:
    ```
    {
      "type": "InvalidCommandError"
    }
    ```

- Reasons for an "InvalidCommandError"
  - The arena is already full
  - The same Websocket connection is being used to register more than one player


## MovePlayerCommand

- payload
```
{"type": "MovePlayerCommand", "data": [(Movement)]}
```
Where movement can be
```
{"move": // (string) either "forward" or "backward"}
{"rotate": // (integer, min: 0, max: 360) number of degress to rotate }
```
There can be only one `move` and one `rotate` per `MovePlayerCommand`.

- when can be sent
  - Only after the player has registerd and the game has started

- expects response
Yes
  - which response
    If the movement went fine
    ```
{
  "type": "MovePlayerAck",
    "data": {
"x": // (integer) X coordinate of the player in the arena after the move,
"y": // (integer) Y coordinate of the player in the arena after the move,
"rotation": // (integer?) angle of the player in the arena after the move
    }
}
    ```

    If the movement failed:
    ```
    {
      "type": "InvalidCommandError"
    }
    ```


- Reasons for an "InvalidCommandError"
  - The game was not started when the command was sent
  - The player was not registered
  - The player hit a wall because of the move
  - The player hit a player because of the move

### PlayerShootCommand

- payload
```
{"type": "PlayerShootComman"}
```
- when can be sent
  - Only after the player has registered and the game has started
- expects response
Yes
  - which response
    If the shot was possible
    ```
{
  "type": "PlayerShotAck",
    "data": {
      "shooting": {
        "tokens": // (integer) shooting tokens the player has,
        "cost": // (integer) the cost of each shot in terms of tokens,
      }
    }
}
    ```

    If the shot failed
    ```
{"type": "InvalidCommandError"}
    ```
- Reasons for an "InvalidCommandError"
  - The game was not started when the command was sent
  - The player was not registered
  - The player run out of tokens

### StartGameCommand

- payload
```
{"type": "StartGameCommand"}
```
- when can be sent
  - When the game hasn't started
- who can send it
  - The controller

- expects response
Yes
  - which response
    If the shot was possible
    ```
{
  "type": "StartGameAck"
}
    ```

    If the game could not be started
    ```
{"type": "InvalidCommandError"}
    ```
- Reasons for an "InvalidCommandError"
  - Who sent the command was not the controller
  - The game was already started

When the game is started the server will also send a `StartGameOrder` order to all registerd players

### ConfigurePlayerCommand

- payload
```
{"type": "ConfigurePlayerCommand", "data": [(Config)]}
```
Where `Config` is:

```
{"op": // (ConfigurationOp), "args": // }
```

Where `ConfigurationOp` can be:

- `radarType`. Valid args for this `ConfigurationOp` are: `long_range_scan`
TODO: make available the switch back to `circular_scan`

- when can be sent
  - When the game has started
  - When the player has registered

- expects response
Yes
  - which response
    If the configuration was possible
    ```
{
  "type": "ConfigurePlayerAck"
}
    ```

    If the player could not be configured
    ```
{"type": "InvalidCommandError"}
    ```
- Reasons for an "InvalidCommandError"
  - The game wasn't started
  - The player wasn't registered
  - The `ConfigurationOp` was invalid

