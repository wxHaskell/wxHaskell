COMPILE=ghc -package wx

ifeq ($(shell uname),Darwin)
%: %.hs
	$(COMPILE) -o $@ $<
	macosx-app $@
else
%: %.hs
	$(COMPILE) -o $@ $<
endif

APPS= BouncingBalls\
      ByeDemo\
      Controls\
      CustomControl\
      FileBrowse\
      Grid\
      HelloWorld\
      ImageViewer\
      Layout\
      ListCtrl\
      ListView\
      Minimal\
      Paint\
      Print\
      Process\
      PropertyGrid    \
      TestTaskBarIcon \
      TimeFlows\
      TimeFlowsEx\

#      DbBrowse\
#      DbConsole\


.PHONY: clean

all: $(APPS)

clean:
	rm -rf $(APPS) *.o *.hi *.app *exe *.manifest
