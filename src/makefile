AS = wla-65816
ASDEF =
LD = wlalink

inflate.ram.bin: inflate.linkfile

inflate.linkfile: inflate.o

inflate.o: inflate.asm inflate.inc

%.o: %.asm
	$(AS) $(addprefix -D,$(ASDEF)) -o $@ $<

%.ram.bin: $<
	$(LD) $(LDFLAGS) -b $< $@

%linkfile: $^
	@echo [objects] > $@
	@$(foreach line,$^,echo $(line) >> $@;)

clean:
	rm *.o *.linkfile *.bin
