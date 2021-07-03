cmake_minimum_required(VERSION 3.10)

# set the project name
project(wxc VERSION 1.0.0.1 DESCRIPTION "A cwrapper library around wxWidgets."
    LANGUAGES CXX)
include(GNUInstallDirs)

# Using SHARED here is a bad practice ??
add_library(wxc SHARED
    src/cpp/apppath.cpp
    src/cpp/dragimage.cpp
    src/cpp/eljaccelerator.cpp
    src/cpp/eljartprov.cpp
    src/cpp/eljaui.cpp
    src/cpp/eljbitmap.cpp
    src/cpp/eljbrush.cpp
    src/cpp/eljbusyinfo.cpp
    src/cpp/eljbutton.cpp
    src/cpp/eljcalendarctrl.cpp
    src/cpp/eljcaret.cpp
    src/cpp/eljcheckbox.cpp
    src/cpp/eljchecklistbox.cpp
    src/cpp/eljchoice.cpp
    src/cpp/eljclipboard.cpp
    src/cpp/eljcoldata.cpp
    src/cpp/eljcolour.cpp
    src/cpp/eljcolourdlg.cpp
    src/cpp/eljcombobox.cpp
    src/cpp/eljconfigbase.cpp
    src/cpp/eljcontrol.cpp
    src/cpp/eljctxhelp.cpp
    src/cpp/eljcursor.cpp
    src/cpp/eljdataformat.cpp
    src/cpp/eljdatetime.cpp
    src/cpp/eljdc.cpp
    src/cpp/eljdcsvg.cpp
    src/cpp/eljdialog.cpp
    src/cpp/eljdirdlg.cpp
    src/cpp/eljdnd.cpp
    src/cpp/eljdrawing.cpp
    src/cpp/eljevent.cpp
    src/cpp/eljfiledialog.cpp
    src/cpp/eljfilehist.cpp
    src/cpp/eljfindrepldlg.cpp
    src/cpp/eljfont.cpp
    src/cpp/eljfontdata.cpp
    src/cpp/eljfontdlg.cpp
    src/cpp/eljframe.cpp
    src/cpp/eljgauge.cpp
    src/cpp/eljgrid.cpp
    src/cpp/eljhelpcontroller.cpp
    src/cpp/eljicnbndl.cpp
    src/cpp/eljicon.cpp
    src/cpp/eljimage.cpp
    src/cpp/eljimagelist.cpp
    src/cpp/eljlayoutconstraints.cpp
    src/cpp/eljlistbox.cpp
    src/cpp/eljlistctrl.cpp
    src/cpp/eljlocale.cpp
    src/cpp/eljlog.cpp
    src/cpp/eljmask.cpp
    src/cpp/eljmdi.cpp
    src/cpp/eljmenu.cpp
    src/cpp/eljmenubar.cpp
    src/cpp/eljmessagedialog.cpp
    src/cpp/eljmime.cpp
    src/cpp/eljminiframe.cpp
    src/cpp/eljnotebook.cpp
    src/cpp/eljpalette.cpp
    src/cpp/eljpanel.cpp
    src/cpp/eljpen.cpp
    src/cpp/eljprintdlg.cpp
    src/cpp/eljprinting.cpp
    src/cpp/eljprocess.cpp
    src/cpp/eljradiobox.cpp
    src/cpp/eljradiobutton.cpp
    src/cpp/eljrc.cpp
    src/cpp/eljregion.cpp
    src/cpp/eljregioniter.cpp
    src/cpp/eljsash.cpp
    src/cpp/eljsplash.cpp
    src/cpp/eljscrollbar.cpp
    src/cpp/eljscrolledwindow.cpp
    src/cpp/eljsingleinst.cpp
    src/cpp/eljsizer.cpp
    src/cpp/eljslider.cpp
    src/cpp/eljspinctrl.cpp
    src/cpp/eljsplitterwindow.cpp
    src/cpp/eljstaticbox.cpp
    src/cpp/eljstaticline.cpp
    src/cpp/eljstatictext.cpp
    src/cpp/eljstatusbar.cpp
    src/cpp/eljsystemsettings.cpp
    src/cpp/eljtextctrl.cpp
    src/cpp/eljtimer.cpp
    src/cpp/eljtipwnd.cpp
    src/cpp/eljtglbtn.cpp
    src/cpp/eljtoolbar.cpp
    src/cpp/eljvalidator.cpp
    src/cpp/eljwindow.cpp
    src/cpp/eljwizard.cpp
    src/cpp/ewxw_main.cpp
    src/cpp/extra.cpp
    src/cpp/stream.cpp
    src/cpp/glcanvas.cpp
    src/cpp/graphicscontext.cpp
    src/cpp/hyperlinkctrl.cpp
    src/cpp/image.cpp
    src/cpp/managed.cpp
    src/cpp/mediactrl.cpp
    src/cpp/pickerctrl.cpp
    src/cpp/previewframe.cpp
    src/cpp/printout.cpp
    src/cpp/sckaddr.cpp
    src/cpp/socket.cpp
    src/cpp/sound.cpp
    src/cpp/stc.cpp
    src/cpp/std.cpp
    src/cpp/taskbaricon.cpp
    src/cpp/textstream.cpp
    src/cpp/treectrl.cpp
    src/cpp/wrapper.cpp
)

set(wxc_PUBLIC_HEADERS
                src/include/dragimage.h
                src/include/eljgrid.h;
                src/include/ewxw_def.h;
                src/include/glcanvas.h;
                src/include/graphicscontext.h;
                src/include/hyperlinkctrl.h;
                src/include/managed.h;
                src/include/mediactrl.h;
                src/include/pickerctrl.h;
                src/include/previewframe.h;
                src/include/printout.h;
                src/include/sound.h;
                src/include/stc.h;
                src/include/stc_gen.h;
                src/include/stream.h;
                src/include/textstream.h;
                src/include/wrapper.h;
                src/include/wxc.h;
                src/include/wxc_def.h;
                src/include/wxc_glue.h;
                src/include/wxc_types.h;
)

set_target_properties(wxc PROPERTIES
    VERSION ${PROJECT_VERSION}
    SOVERSION 1
    PUBLIC_HEADER "${wxc_PUBLIC_HEADERS}"
)

target_include_directories(wxc PRIVATE src/include)

set_property(TARGET wxc PROPERTY
    CXX_STANDARD 14
    CXX_STANDARD_REQUIRED ON)

find_package(wxWidgets REQUIRED gl base core richtext stc webview xrc
    propgrid qa html adv aui ribbon net)

if(wxWidgets_FOUND)
    include(${wxWidgets_USE_FILE})
    target_compile_definitions(wxc PUBLIC ${wxWidgets_DEFINITIONS})
    # and for each of your dependent executable/library targets:
    target_link_libraries(wxc PUBLIC ${wxWidgets_LIBRARIES})
endif()

target_compile_definitions(wxc PUBLIC wxcREFUSE_MEDIACTRL)

install(TARGETS wxc
    LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}
    PUBLIC_HEADER DESTINATION
        ${CMAKE_INSTALL_INCLUDEDIR}/${PROJECT_NAME}-${PROJECT_VERSION_MAJOR}.${PROJECT_VERSION_MINOR}/${PROJECT_NAME})
