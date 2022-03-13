# inflate in 65c816

The decompression code written in 65c816. The code is originally written for SuperFamicom homebrew, but it should be able to be used for other devices with the 65c816 processor.

## Requirements

- [WLA-DX](https://github.com/vhelin/wla-dx)

## Notes

- You can define `UNSAFE` when assembling the code to reduce the code size and make it a little faster, with the restriction where you must locate a sequence of the deflate data within the same bank.
- The code is executed on RAM to self-modify the code for optimization. The inflate routine requires the **0x5DB** bytes with `UNSAFE`, and **0x68B** without.
- It requires **0x46A** bytes for variables. These must be located in the same bank as the inflate routine on RAM.
- It requires **$00-$13** (included) for variables in **Direct Page**. You can set the arbitrary DP register value before calling the inflate routine.
- The interruption may occur during the decompression if handled appropriately.
- As the inflate routine is located on RAM, once you assemble the inflate.asm in /src directory, you can just include inflate.ram.bin into your source code with your favourite assembler.

## Usage

- Set the RAM address where you want to locate inflate routine in `/src/inflate.inc`
- Assemble `/src/inflate.asm`, which outputs inflate.ram.bin
- Include inflate.ram.bin in the your source code and write the code to copy the binary onto the same RAM address where you first configured.
- Include deflated-data with [zopfli](https://github.com/google/zopfli) with the `--deflate` option
- Call `JSL Inflate` with the condition:
  - A : source bank << 8 | destination bank
  - X : source address
  - Y : destination address

## Example

The SuperFamicom homebrew ROM is provided as an example. To assemble, run the following:

```
cd src && make
cd ../example && make
```
