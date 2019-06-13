Final Fantasy Legend II Memory Map
==================================

Cartridge
---------

- Mapper: MBC2.
- ROM: 256 KiB (16 banks of 16 KiB each)..
- RAM: 512 nybbles.
- GBC Support: No.
- SGB Support: No.

ROM
---

TODO

- Bank `$0` (`ROM0:$0000 .. ROM0:$3FFF`)
  - Fixed ROM bank. Contains RST vectors, ROM header, entry point, common code/data, etc.
- Bank `$1` (`ROM1:$4000 .. ROM1:$7FFF`):
- Bank `$2` (`ROM2:$4000 .. ROM2:$7FFF`): Background graphics.
- Bank `$3` (`ROM3:$4000 .. ROM3:$7FFF`): Sprite graphics.
- Bank `$4` (`ROM4:$4000 .. ROM4:$7FFF`): ...
- Bank `$5` (`ROM5:$4000 .. ROM5:$7FFF`): ...
- Bank `$6` (`ROM6:$4000 .. ROM6:$7FFF`): ...
- Bank `$7` (`ROM7:$4000 .. ROM7:$7FFF`)
  - `$7000` - tileset types (1 byte per type, 32 bytes total).
  - `$7800` - tileset tiles (1 byte per index, 32 bytes total).
    - tile graphic address = ROM2:($4000 + index * 64)
- Bank `$8` (`ROM8:$4000 .. ROM8:$7FFF`):
- Bank `$9` (`ROM9:$4000 .. ROM9:$7FFF`):
- Bank `$A` (`ROMA:$4000 .. ROMA:$7FFF`):
- Bank `$B` (`ROMB:$4000 .. ROMB:$7FFF`):
- Bank `$C` (`ROMC:$4000 .. ROMC:$7FFF`):
- Bank `$D` (`ROMD:$4000 .. ROMD:$7FFF`):
- Bank `$E` (`ROME:$4000 .. ROME:$7FFF`):
- Bank `$F` (`ROMF:$4000 .. ROMF:$7FFF`):

RAM
---

TODO

- `$C450` - current tileset
