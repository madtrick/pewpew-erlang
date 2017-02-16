# Notifications

Following are the available notifications:

- `GameSnapshotNotification`
- `NoSlotsLeftNotification`
- `PlayerDestroyedNotification`
- `PlayerHitByShotNotification`
- `RadarScanNotification`


## GameSnapshotNotification

- payload
```
{
  "type": "GameSnapshotNotification",
    "data": {
      "arena_snapshot": {
        "players:_snapshots": [(PlayerSnapshot)],
        "shots_snapshots": []
      }
    }
}
```

Where `PlayerSnapshot` is:

```
{
  "id": // (string) Player ID,
    "coordinates": {
      "x": // (float) X coordinate of the player in the arena,
      "y": // (float) Y coordinate of the player in the arena
    }
  "rotation": // (integer) Angle of the player in the arena ,
  "life": // (integer) Life units left on the player,
  "radar": {
    "type": // (string) Radar type. One of ['longScan', 'circular_scan'],
    "radius": // (integer) Radius of the radar
  }
}
```

Where `ShotSnapshot` is:

```
{
  "id": // (integer) Shot ID,
    "coordinates": {
      "x": // (float) X coordinate of the player in the arena,
      "y": // (float) Y coordinate of the player in the arena
    }
}
```

- when is sent
Its sent on every game cycle through the control Websocket connection

## NoSlotsLeftNofication

- payload

```
{"type": "NoSlotsLeftNotification"}
```

- when is sent
When a player tries to register but the arena is already full

## PlayerDestroyedNotification

- payload
```
{"type": "PlayerDestroyedNotification"}
```

- when is sent
When the player's life runs out

## PlayerHitByShotNotification

- payload
```
{
  "type": "PlayerHitByShotNotification",
    "data": {
      "life": // (integer) Life units left on the player
    }
}
```

- when is sent
When the player is hit by a shot

## RadarScanNotification

- payload
```
{
  "type": "RadarScanNotification",
    "data": {
      "elements": [(ScannedElement)],
      "walls": [(ScannedWall)]
    }
}
```

Where `ScannedElement` is:

```
{
  "type": "unknown",
    "coordinates": {
      "x": // (float) X coordinate of the player in the arena,
      "y": // (float) Y coordinate of the player in the arena
    }
}
```

Where `ScannedWall` is:

```
[
  // (float) X coordinate of the intersection of the scan with the wall,
  // (float) Y coordinate of the intersection of the scan with the wall
]
```
