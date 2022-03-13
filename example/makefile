AS = wla-65816
ASDEF =
LD = wlalink
LDFLAGS = -v -d -S

test.sfc: test.linkfile
	$(LD) $(LDFLAGS) $< $@

test.linkfile: test.o

test.o: test.asm test.inc ../src/inflate.inc

%.o: %.asm
	$(AS) $(addprefix -D,$(ASDEF)) -o $@ $<

%.ram.bin: $<
	$(LD) $(LDFLAGS) -b $< $@

%linkfile: $^
	@echo [objects] > $@
	@$(foreach line,$^,echo $(line) >> $@;)

clean:
	rm *.o *.linkfile *.sym *.sfc
