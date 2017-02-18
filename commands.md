# Commands

Following are the available commands:

- `RegisterPlayerCommand`
- `MovePlayerCommand`
- `PlayerShootCommand`
- `StartGameCommand`
- `ConfigurePlayerCommand`

## RegisterPlayerCommand

- When can be sent
 - At any time during the game. But depending on the value of the `players_can_join_started_game` configuration variable they might not be able to participate on an ongoing game

- Payload
```
{"type": "RegisterPlayerCommand"}
```

- Response
    - If the registration was successful:

    ```
    {
      "type": "RegisterPlayerAck",
        "data": {
          "id": // (string) Player ID
          "x": // (integer) X coordinate of the player in the arena
          "y": // (integer) Y coordinate of the player in the arena
          "life": // (integer) Life units left on the player
          "shooting": // (ShootingInfo) Details about the shooting capabilities of the player
        }
    }
    ```

    Where `ShootingInfo` is:

    ```
    {
      "tokens": // (integer) shooting tokens the player has
      "cost": // (integer) the cost of each shot in terms of tokens
    }
    ```

    If the player can also join an ongoing game then he will also receive:

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

- Reasons for an `InvalidCommandError`
  - The arena is already full
  - The same Websocket connection is being used to register more than one player


## MovePlayerCommand

- When can be sent
  - Only after the player has registerd and the game has started

- Payload

```
{"type": "MovePlayerCommand", "data": [(Movement), (Rotation)]}
```

Where `Movement` is:

```
{"move": // (string) either "forward" or "backward"}
```

and `Rotation` is:

```
{"rotate": // (integer, min: 0, max: 360) number of degress to rotate }
```

There can be only one `move` and one `rotate` per `MovePlayerCommand`.

- Response
  - If the command was valid

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

  - If the command failed:
    ```
    {
      "type": "InvalidCommandError"
    }
    ```

- Reasons for an `InvalidCommandError`
  - The game was not started when the command was sent
  - The player was not registered
  - The player hit a wall because of the move
  - The player hit a player because of the move

## PlayerShootCommand

- when can be sent
  - Only after the player has registered and the game has started

- Payload
```
{"type": "PlayerShootComman"}
```

- Response
  - If the command was valid

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

  - If the command failed
    ```
    {"type": "InvalidCommandError"}
    ```

- Reasons for an `InvalidCommandError`
  - The game was not started when the command was sent
  - The player was not registered
  - The player run out of tokens

## StartGameCommand

- When can be sent
  - When the game hasn't started

- Payload

```
{"type": "StartGameCommand"}
```

- Who can send it
  - The controller bot

- Response
  - If the command was valid

    ```
    {
      "type": "StartGameAck"
    }
    ```

  - If the command failed
    ```
    {"type": "InvalidCommandError"}
    ```

- Reasons for an `InvalidCommandError`
  - Who sent the command was not the controller
  - The game was already started

When the game is started the server will also send a `StartGameOrder` order to all registerd players.

## ConfigurePlayerCommand

- When can be sent
  - At any point in time

- Payload
```
{"type": "ConfigurePlayerCommand", "data": [(Config)]}
```

Where `Config` is:

```
{"op": // (ConfigurationOp), "args": // }
```

Where `ConfigurationOp` can be:

- `radarType`. Valid args for this `ConfigurationOp` are: `long_range_scan`


- Response
  - If the command was valid

    ```
    {
      "type": "ConfigurePlayerAck"
    }
    ```

  - If the command failed
    ```
    {"type": "InvalidCommandError"}
    ```

- Reasons for an `InvalidCommandError`
  - The `ConfigurationOp` was invalid

