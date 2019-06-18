#!/bin/sh
xxd ffl2.gb > ffl2.hex
xxd ffl2-orig.gb > ffl2-orig.hex
diff ffl2.hex ffl2-orig.hex