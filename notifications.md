# Notifications

Following are the available notifications:

- `GameSnapshotNotification`
- `NoSlotsLeftNotification`
- `PlayerDestroyedNotification`
- `PlayerHitByShotNotification`
- `RadarScanNotification`


## GameSnapshotNotification

- When is it sent
  - Its sent on every game cycle

- Who can receive it
  - The controller

- Payload

  ```
  {
    "type": "GameSnapshotNotification",
      "data": {
        "arena_snapshot": {
          "players:_snapshots": [(PlayerSnapshot)],
          "shots_snapshots": [(ShotSnapshot)]
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

## NoSlotsLeftNofication

- When is it sent
  - When a player tries to register but the arena is already full

- Who can receive it
  - A bot

- Payload

  ```
  {"type": "NoSlotsLeftNotification"}
  ```

## PlayerDestroyedNotification

- When is it sent
  - When the player's life runs out

- Who can receive it
  - A bot

- payload
```
{"type": "PlayerDestroyedNotification"}
```

## PlayerHitByShotNotification

- When is it sent
  - When the player is hit by a shot

- Who can receive it
  - A bot

- Payload

  ```
  {
    "type": "PlayerHitByShotNotification",
      "data": {
        "life": // (integer) Life units left on the player
      }
  }
  ```

## RadarScanNotification

- When is it sent
  - On every cycle

- Who can receive it
  - A bot

- Payload

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
