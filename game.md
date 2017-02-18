## Game

Game moves forward in the sense of cycles. Each cycle has a fixed time duration on which the game engine
will evaluate the commands sent by the bots and react to them. At the end of each cycle the engine
will

- Send a response to each bot who sent a command.
- Send a notification to each bot with result of the radar sweep.
- Send a notification to the controller with information about each one for the bots in the
arena.

Notice that bots can only send one message on each game cycle. Additional messages will be silently discarded.

### Shooting

Bots can send a command to shoot as long as they have shooting tokens and they are enough to "pay"
for the cost of the shot. On each cycle the number of shooting tokens increases in a fixed amount.

When a bot is hit, it will receive a notification indicating its
remaining life. If the bot is hit enough times to consume all his life it will die on which case the engine
will send a notification to the bot.

### Radars

Bots have a radar that will sweep for nearby objects (bots and walls). The radar mode
can be alternated between `circular_scan` (default) and `long_range`:

- `circular_scan` does a scan around the bot but with a short radius
- `long_range_scan` scans only on the direction the bot is moving with an opening of 30 degrees left and right.
On this case the radar can scan further away than the `circular_scan`. Note that right now is not possible to
go back to the `circular_scan` mode.
