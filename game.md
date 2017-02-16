## Game

When the game has started players will be able to send one com
Game moves forward in the sense of cycles. Each cycle a fix time duration on which the game engine
will evaluate the commands sent by the players and react to them. At the end of each cycle the engine
will also send a notification to the controller with information about each one for the player in the
arena.

Notice that players can only send one message on each game cycle. Additional messages will be silently discarded.

### Shooting

Players can send a command to shoot as long as they have shooting tokens and they are enough to "pay"
for the cost of the shot. On each cycle the number of shooting tokens increases in a fixed amount.

When a player is hit, it will receive a notification indicating the
remaining life. If the player is hit enough times to consume all his life it will die on which case the engine
will send a notification to the player.

### Radars

Players have a radar that will sweep for nearby objects (players and walls) and notify about it. The radar mode
can be alternated between `circular_scan` (default) and `long_range`:

- `circular_scan` does a scan around the player but with a short radius
- `long_range_scan` scans only on the direction the player is moving with an opening of 30 degrees left and right.
On this case the radar can scan further away than the `circular_scan`. Note that right now is not possible to
go back to the `circular_scan` mode.
