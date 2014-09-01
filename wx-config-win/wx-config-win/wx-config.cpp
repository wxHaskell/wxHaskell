////////////////////////////////////////////////////////////////////////////////////////////////////
// Name:        wx-config-win.cpp
// Purpose:     A wx-config implementation for Windows
// Author:      Takeshi Miya
// Created:     2006-03-23
// Copyright:   (c) Takeshi Miya, 2006
//              (c) J. O'Donoghue, 2012
// Licence:     wxWidgets licence
////////////////////////////////////////////////////////////////////////////////////////////////////

// Allows building under VS2010
#undef UNICODE
#pragma warning( disable : 4996 )

#include <iostream>
#include <string>
#include <cstdlib>

#include <map>
#include <fstream>
#include <algorithm>
#include <vector>

#include <windows.h>

#define ARRAY_LENGTH(a) (sizeof(a) / sizeof(a[0]))

// -------------------------------------------------------------------------------------------------

std::string usageText[] = {
  "Usage: wx-config [--prefix[=DIR]] [--wxcfg[=DIR]] [--release], [--version]",
  "                 [--universal[=yes|no]] [--unicode[=yes|no]] [--static[=yes|no]]",
  "                 [--debug[=yes|no] [--basename] [--cc] [--cxx] [--cflags]",
  "                 [--cxxflags] [--rcflags] [--ld] [-v] [--compiler[=gcc|dmc|vc]",
  "                 [--libs [LIBS...]"
  "",
  "",
  "  --prefix[=DIR]          Path to the wxWidgets installation (ie. C:\\wxWidgets)",
  "  --wxcfg[=DIR]           Relative path to build.cfg file (ie. gcc_dll\\mswud)",
  "  --cflags                Outputs all pre-processor and compiler flags.",
  "  --cxxflags              Same as --cflags but for C++.",
  "  --rcflags               Outputs all resource compiler flags. [UNTESTED]",
  "  --libs [LIBS...]        Outputs library names to use - see below.",
  "",
  "  --debug[=yes|no]        Uses a debug configuration if found.",
  "  --unicode[=yes|no]      Uses an unicode configuration if found.",
  "  --static[=yes|no]       Uses a static configuration if found.",
  "  --universal[=yes|no]    Uses an universal configuration if found.",
  "  --compiler[=gcc,dmc,vc] Selects the compiler.",
  "  --release               Outputs the wxWidgets release number.",
  "  --version               Outputs the wxWidgets version.",
  "  --basename              Outputs the base name of the wxWidgets libraries.",
  "  --cc                    Outputs the name of the C compiler.",
  "  --cxx                   Outputs the name of the C++ compiler.",
  "  --ld                    Outputs the linker command.",
  "  -v                      Outputs the revision of wx-config.",
  "",
  "",
  "  Note that using --prefix is not needed if you have defined the",
  "  environmental variable WXWIN.",
  "",
  "  Note that using --wxcfg is not needed if you have defined the",
  "  environmental variable WXCFG."
  "",
  "  The LIBS arguments (comma or space separated) may be used to specify",
  "  the wxWidgets libraries that you wish to use in your application. The",
  "  \"std\" label may be used to import all libraries which would be used if",
  "  none were specified explicitly, e.g. wx-config --libs core,base. The \"all\"",
  "  label may be used to import all libraries found to be installed.",
  ""
};

// The full list of libs in a standard wxWidgets build. This is ordered so that
// static linking will work
std::string allLibs_a[] = 
{
  "xrc", "stc", "richtext", "ribbon", "propgrid", "aui", "gl", "media", "html",
  "webview", "qa", "adv", "core", "xml", "net", "base"
};
std::vector<std::string> allLibs(allLibs_a, allLibs_a + ARRAY_LENGTH(allLibs_a));

// The libraries you get if you ask for "std". This is ordered so that static
// linking will work
std::string stdLibs_a[] =
{
  "xrc", "html", "qa", "adv", "core", "xml", "net", "base"
};
std::vector<std::string> stdLibs(stdLibs_a, stdLibs_a + ARRAY_LENGTH(stdLibs_a));

void usage() 
{
  // Print error text
  for (int i = 0; i < ARRAY_LENGTH(usageText); i++)
  {
    std::cerr << usageText[i] << std::endl;
  }
}

bool contains(std::vector<std::string> vec, std::string val)
{
  std::vector<std::string>::iterator it = vec.begin();
  bool result = false;

  if (vec.size() == 0)
    return result;

  do
  {
    result = (*it == val);
    ++it;
  } while (!result && it != vec.end());

  return result;
}

std::vector<std::string> replace(std::vector<std::string> vec, std::string old_val, std::vector<std::string> new_vals)
{
  std::vector<std::string> result;
  std::vector<std::string>::iterator it;

  for (it = vec.begin(); it != vec.end(); ++it)
  {
    if (*it == old_val)
    {
      std::vector<std::string>::iterator jt;
      for (jt = new_vals.begin(); jt != new_vals.end() ; ++jt)
      {
        result.push_back(*jt);
      }
    }
    else
    {
      result.push_back(*it);
    }
  }
  
  return result;
}

std::string getSvnRevision()
{
  std::string str = "$Rev: 26 $";
  if (str.length() > 8)
    return str.substr(6, str.length()-8);
  else
    return "X";
}

std::string getSvnDate()
{
  std::string str = "$Date: 2006-12-21 02:08:16 +0000 (Thu, 21 Dec 2006) $";
  if (str.length() > 16)
    return str.substr(7, 10);
  else
    return "2012-04-04";
}

static const std::string g_tokError = "wx-config Error: ";
static const std::string g_tokWarning = "wx-config Warning: ";

// -------------------------------------------------------------------------------------------------

/// Program options
class Options
{
public:
  std::string& operator[](const std::string& key)
  {
    return m_vars[key];
  }

  bool keyExists(const std::string& key) const
  {
    return m_vars.count(key) != 0;
  }
    
  const std::string& keyValue(const std::string& key) const
  {
    return m_vars.find(key)->second;
  }

  std::map<std::string,std::string>& getVars()
  {
    return m_vars;
  }
protected:
    std::map<std::string,std::string> m_vars;
};

// -------------------------------------------------------------------------------------------------

/// File build.cfg/config.* options
class BuildFileOptions : public Options
{
public:
  BuildFileOptions(const std::string& filepath)
  {
    parse(filepath);
  }
  
  bool parse(const std::string& filepath)
  {
    std::string line;
    std::ifstream file(filepath.c_str());
    if (file.is_open())
    {
      while (!file.eof() )
      {
        std::getline(file, line);

        // it's a comment line
        if (line.find_first_of('#') != std::string::npos)
          continue;

        // strip spaces
        line.erase( std::remove(line.begin(), line.end(), ' '), line.end() );
        
        split(line);
      }
      file.close();

      if (!m_vars.empty())
        return true;
    }
    else
      std::cout << g_tokError << "Unable to open file '" << filepath.c_str() << "'." << std::endl;

    return false;
  }

protected:
  void split(const std::string& line)
  {
    size_t sep = line.find('=');
    if (sep != std::string::npos)
    {
      std::string key = line.substr(0, sep);
      std::string val = line.substr(sep+1, line.size()-sep-1);
      m_vars[key] = val;
    }
  }
};

// -------------------------------------------------------------------------------------------------

/// File setup.h options
class SetupHOptions
{
public:
  bool& operator[](const std::string& key)
  {
    return m_vars[key];
  }

  bool keyExists(const std::string& key) const
  {
    return m_vars.count(key) != 0;
  }
  
  bool keyValue(const std::string& key) const
  {
    return m_vars.find(key)->second;
  }
  
  typedef std::map<std::string,bool> StringBoolMap;
    
  StringBoolMap& getVars()
  {
    return m_vars;
  }
protected:
  StringBoolMap m_vars;
  
  
public:
  SetupHOptions(const std::string& filepath)
  {
    parse(filepath);
  }
  
  bool parse(const std::string& filepath)
  {
    std::string line;
    std::ifstream file(filepath.c_str());
    if (file.is_open())
    {
      while (!file.eof())
      {
        std::getline(file, line);
        
        // does the splitting/parsing
        split(line);
      }
      file.close();
      
      if (!m_vars.empty())
        return true;
    }
    else
      std::cout << g_tokError << "Unable to open file '" << filepath.c_str() << "'." << std::endl;
    
    return false;
  }
    
  void printDebug()
  {
    std::cout << "DEBUG: setup.h contents BEGIN -------------------------------------------------" << std::endl;
    for (StringBoolMap::iterator it = m_vars.begin(); it != m_vars.end(); ++it)
      std::cout << it->first << "=" << it->second << std::endl;
    std::cout << "DEBUG: setup.h contents END ---------------------------------------------------" << std::endl;
  }

protected:
  void split(std::string& line)
  {
    // it's a comment line
    if (line.find_first_of('/') != std::string::npos)
      return; // skips the line
    
    // strip spaces and tabs
    line.erase( std::remove(line.begin(), line.end(), ' '), line.end() );
    line.erase( std::remove(line.begin(), line.end(), '\t'), line.end() );
    
    std::string tokDefine("#define");
    size_t posDefine = line.find_first_of(tokDefine);
    
    // it's a #define line
    if (posDefine != std::string::npos)
    {
      std::string key;
      bool val;
      
      // resolves val, checking if last char is 0 or 1
      char lastChar = line.at(line.length()-1);       // TODO: I don't like this line :P
      if (lastChar == '0')
        val = false;
      else if (lastChar == '1')
        val = true;
      else
        return; // skips the line
      
      // resolves key
      size_t startPos = posDefine + tokDefine.length();
      key = line.substr(startPos, line.length() - startPos - 1);
      
      // finally saves the parsed data!
      m_vars[key] = val;
    }        
  }
};


// -------------------------------------------------------------------------------------------------

/// Command line options
class CmdLineOptions : public Options
{
public:
  CmdLineOptions(int argc, char* argv[])
  {
    parse(argc, argv);
  }
  
  bool validArgs()
  {
    bool valid = keyExists("--compiler") ||
                 keyExists("--variable") ||
                 keyExists("--define-variable") ||
                 keyExists("--prefix") ||
                 keyExists("--wxcfg") ||
                 keyExists("--libs") ||
                 keyExists("--cflags") ||
                 keyExists("--cxxflags") ||
                 keyExists("--cppflags") ||
                 keyExists("--rcflags") ||
                 keyExists("--list") ||
                 keyExists("--debug") ||
                 keyExists("--unicode") ||
                 keyExists("--static") ||
                 keyExists("--universal") ||
                 keyExists("--release") ||
                 keyExists("--version") ||
                 keyExists("--basename") ||
                 keyExists("--cc") ||
                 keyExists("--cxx") ||
                 keyExists("--ld") ||
                 keyExists("-v");

    // TODO: not all flags are being validated
    if (!valid)
    {
      if (m_vars.size() > 1 && !keyExists("--help"))
        std::cout << g_tokError << "Unrecognised option: '" << m_vars.begin()->first << "'\n" << std::endl;
      
      usage();
    }

    return valid;
  }

  bool parse(int argc, char* argv[])
  {
    for(int i=0; i<argc; ++i)
    {
      std::string line;
      line = argv[i];
      split(line);
    }
    parseLibs(argv[argc-1]);
    
    return true;
  }
  
  std::vector<std::string> getLibs() const
  {
    return m_libs;
  }
  
protected:

  void split(const std::string& line)
  {
    size_t sep = line.find("=");
    if (sep != std::string::npos)
    {
      std::string key = line.substr(0, sep);
      std::string val = line.substr(sep+1, line.size()-sep-1);
      m_vars[key] = val;
    }
    else
      m_vars[line] = "";
  }

  bool libExists(const std::string& lib)
  {
    return std::find(m_libs.begin(), m_libs.end(), lib) != m_libs.end();
  }

  void addLib(const std::string& lib)
  {
    // adds the lib if its not present already
    if (!libExists(lib))
      m_libs.push_back(lib);
  }

  void parseLibs(const std::string libs)
  {
    std::string param = libs;
    
    // if the last parameter doesn't haves a -- switch
    if (param.find("--") == std::string::npos)
    {
      // saves in the vector, comma separated text like "text1,text2,text3,text4"
      while(true)
      {
        size_t comma = param.find(",");
        if (comma != std::string::npos)
        {
          m_libs.push_back(param.substr(0, comma));
          param = param.substr(comma+1, param.size()-comma-1);
        }
        else
        {
          m_libs.push_back(param);
          break;
        }
      }
    }
    
    // assuming magic keyword 'std' as a lib parameter for non-monolithic
    // magic keyword std: links with libraries defined in stdLibs
    // Note that we cannot deal with the 'all' magic keyword here because
    // we do not yet know what 'all' the libraries present are :-(
    // If no list of libraries, assume we want "std"
    if (m_libs.empty())
    {
      for (int l = 0; l < ARRAY_LENGTH(stdLibs); ++l)
      {
        addLib(stdLibs[l]);
      }
    }
  }

  std::vector<std::string> m_libs;
};

// -------------------------------------------------------------------------------------------------

/// Struct to keep programs
struct CompilerPrograms
{
  std::string cc; // C compiler
  std::string cxx; // C++ compiler
  std::string ld; // dynamic libs linker
  std::string lib; // static libs linker
  std::string windres; // resource compiler
};

/// Struct to keep switches
struct CompilerSwitches
{
  std::string includeDirs;   // -I
  std::string resIncludeDirs;   // --include-dir
  std::string libDirs;       // -L
  std::string linkLibs;      // -l
  std::string libPrefix; // lib
  std::string libExtension; // a
  std::string defines;       // -D
  std::string resDefines;    // --define
  std::string genericSwitch; // -
  std::string objectExtension; // o
  bool forceLinkerUseQuotes; // use quotes for filenames in linker command line (needed or not)?
  bool forceCompilerUseQuotes; // use quotes for filenames in compiler command line (needed or not)?
  bool linkerNeedsLibPrefix; // when adding a link library, linker needs prefix?
  bool linkerNeedsLibExtension; // when adding a link library, linker needs extension?
  bool supportsPCH; // supports precompiled headers?
  std::string PCHExtension; // precompiled headers extension
};

static bool g_sEasyMode = false;

void checkEasyMode(CmdLineOptions& cl)
{
  if (cl.keyExists("--easymode"))
  {
    if (cl["--easymode"] == "no")
      g_sEasyMode = false;
    else if (cl["--easymode"] == "yes" || cl["--easymode"].empty())
      g_sEasyMode = true;
  }
}

// -------------------------------------------------------------------------------------------------

// Look at the directory indicated by WXCFG to determine what libraries are *actually*
// installed. The Unix version doesn't need to do this because it already knows what was
// configured (it was 'burned' into wx-config as part of the configure process), whereas
// we don't have that luxury

std::vector<std::string> getInstalledLibs(Options& po)
{
  // Note - we need only the first directory in WXCFG.
  std::string libDir = po["wxcfg"].substr(0,po["wxcfg"].find("\\"));
  std::string cfgWildcard = po["prefix"] + "\\lib\\" + libDir + "\\libwx*_*.a";
  TCHAR* szFindFiles = strncpy(new char[MAX_PATH], cfgWildcard.c_str(), MAX_PATH);
  std::vector<std::string>* pLibsList = new std::vector<std::string>;
  WIN32_FIND_DATA ffd;
  
  // Use WIN32 API to find all wxWidgets implibs (i.e. libwx*_**.a), as this is what gcc wants
  // Note that this will 'fail' to find the base library, so we will add it back later (a valid
  // install must contain base)
  HANDLE hDir = FindFirstFile(szFindFiles, &ffd);

  if (hDir != INVALID_HANDLE_VALUE)
  {
    do 
    {
      if (!(ffd.dwFileAttributes & FILE_ATTRIBUTE_DIRECTORY))
      {
        std::string fn(ffd.cFileName);
        pLibsList->push_back(fn);
      }
    } while (FindNextFile(hDir, &ffd) != 0);
  }
  FindClose(hDir);
  
  // Now filter out for the library names, we want the part immedately after the '_' character,
  // up to (and excluding) the '.' character.
  std::vector<std::string>::iterator it;
  std::vector<std::string> libs;
  
  for (it = pLibsList->begin(); it != pLibsList->end(); ++it)
  {
    std::string i = *it;
    std::string name = i.substr(i.find("_") + 1);
    name = name.substr(0, name.find("."));
    libs.push_back(name);
  }

  // Add back base
  libs.push_back("base");

  delete pLibsList;
  return libs;
}

/// Compiler abstract base class
class Compiler
{
public:
  Compiler(const std::string& name) : m_name(name) {}
  
  std::string easyMode(const std::string& str)
  {
    if (g_sEasyMode)
      return str;
    else
      return std::string();
  }
  
  std::string addFlag(const std::string& flag)
  {
    if (flag.empty())
      return "";
    return flag + " ";
  }

  std::string addLib(const std::string& lib)
  {
    std::string result;
    result = m_switches.linkLibs;
    
    if (m_switches.linkerNeedsLibPrefix)
      result += m_switches.libPrefix;
    
    result += lib;
    
    if (m_switches.linkerNeedsLibExtension)
      result += "." + m_switches.libExtension;

    result += " ";

    if (lib.empty())
      return "";
    return result;
  }

  std::string addDefine(const std::string& define)
  {
    if (define.empty())
      return "";
    return m_switches.defines + define + " ";
  }

  std::string addResDefine(const std::string& resDefine)
  {
    if (resDefine.empty())
      return "";
    return m_switches.resDefines + " " + resDefine + " ";
  }

  std::string addIncludeDir(const std::string& includeDir)
  {
    if (includeDir.empty())
      return "";
    return m_switches.includeDirs + includeDir + " ";
  }

  std::string addLinkerDir(const std::string& libDir)
  {
    if (libDir.empty())
      return "";
    return m_switches.libDirs + libDir + " ";
  }

  std::string addResIncludeDir(const std::string& resIncludeDir)
  {
    if (resIncludeDir.empty())
      return "";
    return m_switches.resIncludeDirs + " " + resIncludeDir + " ";
  }

  std::string getName() const
  {
    return m_name;
  }
  
  // Configure "base" library link libs (multilib.bkl#24)
  void process_base(Options& po, BuildFileOptions& cfg)
  {
    if (cfg["MONOLITHIC"] == "0")
      po["__WXLIB_BASE_p"] = addLib(po["LIB_BASENAME_BASE"]);
  }

  // Configure "net" library link libs (multilib.bkl#48, depends: base)
  void process_net(Options& po, BuildFileOptions& cfg)
  {
    if (cfg["MONOLITHIC"] == "0")
      po["__WXLIB_NET_p"] = addLib(po["LIB_BASENAME_BASE"] + "_net");
  }

  // Configure "XML" library link libs (multilib.bkl#238, depends: base, $(EXTRALIBS_XML))
  void process_xml(Options& po, BuildFileOptions& cfg)
  {
    if (cfg["MONOLITHIC"] == "0")
      po["__WXLIB_XML_p"] = addLib(po["LIB_BASENAME_BASE"] + "_xml");
  }

  // Configure "core" library link libs (multilib.bkl#72, depends: base)
  void process_core(Options& po, BuildFileOptions& cfg)
  {
    if (cfg["MONOLITHIC"] == "0" && cfg["USE_GUI"] == "1")
      po["__WXLIB_CORE_p"] = addLib(po["LIB_BASENAME_MSW"] + "_core");
  }

  // Configure "adv" library link libs (multilib.bkl#98, depends: core, base)
  void process_adv(Options& po, BuildFileOptions& cfg)
  {
    if (cfg["MONOLITHIC"] == "0" && cfg["USE_GUI"] == "1")
      po["__WXLIB_ADV_p"] = addLib(po["LIB_BASENAME_MSW"] + "_adv");
  }

  // Configure "qa" library link libs (multilib.bkl#213, depends: core, base, xml)
  void process_qa(Options& po, BuildFileOptions& cfg)
  {
    if (cfg["MONOLITHIC"] == "0" && cfg["USE_GUI"] == "1" && cfg["USE_QA"] == "1")
    {
      po["__WXLIB_QA_p"] = addLib(po["LIB_BASENAME_MSW"] + "_qa");
      po["__WXLIB_XML_p"] = addLib(po["LIB_BASENAME_BASE"] + "_xml");
    }
  }

  // Configure "xrc" library link libs (multilib.bkl#262, depends: adv, core, xml, base)
  void process_xrc(Options& po, BuildFileOptions& cfg, SetupHOptions& sho)
  {
    if (cfg["MONOLITHIC"] == "0" && cfg["USE_GUI"] == "1" && (cfg["USE_XRC"] == "1" || sho["wxUSE_XRC"]))
    {
      po["__WXLIB_XRC_p"] = addLib(po["LIB_BASENAME_MSW"] + "_xrc");
      po["__WXLIB_XML_p"] = addLib(po["LIB_BASENAME_BASE"] + "_xml");
      po["__WXLIB_ADV_p"] = addLib(po["LIB_BASENAME_MSW"] + "_adv");
    }
  }

  // Configure "aui" library link libs (multilib.bkl#289, depends: adv, core, base)
  // in this one, for some reason there is no USE_AUI in BuildFileOptions, so we need
  // to look in SetupHOPtions.
  void process_aui(Options& po, BuildFileOptions& cfg, SetupHOptions& sho)
  {
    if (cfg["MONOLITHIC"] == "0" && (cfg["USE_AUI"] == "1" || sho["wxUSE_AUI"]))
    {
      po["__WXLIB_AUI_p"] = addLib(po["LIB_BASENAME_MSW"] + "_aui");
      po["__WXLIB_ADV_p"] = addLib(po["LIB_BASENAME_MSW"] + "_adv");
    }
  }
  
  // Configure "ribbon" library link libs (multilib.bkl#314, depends: adv, core, base)
  // in this one, for some reason there is no USE_RIBBON in BuildFileOptions, so we need
  // to look in SetupHOPtions.
  void process_ribbon(Options& po, BuildFileOptions& cfg, SetupHOptions& sho)
  {
    if (cfg["MONOLITHIC"] == "0" && (cfg["USE_RIBBON"] == "1" || sho["wxUSE_RIBBON"]))
    {
      po["__WXLIB_RIBBON_p"] = addLib(po["LIB_BASENAME_MSW"] + "_ribbon");
      po["__WXLIB_ADV_p"] = addLib(po["LIB_BASENAME_MSW"] + "_adv");
    }
  }

  // Configure "propgrid" library link libs (multilib.bkl#339, depends: adv, core, base)
  // in this one, for some reason there is no USE_PROPGRID in BuildFileOptions, so we need
  // to look in SetupHOPtions.
  void process_propgrid(Options& po, BuildFileOptions& cfg, SetupHOptions& sho)
  {
    if (cfg["MONOLITHIC"] == "0" && (cfg["USE_PROPGRID"] == "1" || sho["wxUSE_PROPGRID"]))
    {
      po["__WXLIB_PROPGRID_p"] = addLib(po["LIB_BASENAME_MSW"] + "_propgrid");
      po["__WXLIB_ADV_p"] = addLib(po["LIB_BASENAME_MSW"] + "_adv");
    }
  }
  
  // Configure "richtext" library link libs (multilib.bkl#364, depends: adv, $(htmldll_library_link), xml, core, base)
  // in this one, for some reason there is no USE_RICHTEXT in BuildFileOptions, so we need
  // to look in SetupHOPtions.
  void process_richtext(Options& po, BuildFileOptions& cfg, SetupHOptions& sho)
  {
    if (cfg["MONOLITHIC"] == "0" && (cfg["USE_RICHTEXT"] == "1" || sho["wxUSE_RICHTEXT"]))
    {
      po["__WXLIB_RICHTEXT_p"] = addLib(po["LIB_BASENAME_MSW"] + "_richtext");
      po["__WXLIB_ADV_p"] = addLib(po["LIB_BASENAME_MSW"] + "_adv");
      po["__WXLIB_XML_p"] = addLib(po["LIB_BASENAME_MSW"] + "_xml");
    }
  }

  // Configure "stc" library link libs (multilib.bkl#395, depends: core, base)
  // in this one, for some reason there is no USE_STC in BuildFileOptions, so we need
  // to look in SetupHOPtions.
  void process_stc(Options& po, BuildFileOptions& cfg, SetupHOptions& sho)
  {
    if (cfg["MONOLITHIC"] == "0" && (cfg["USE_STC"] == "1" || sho["wxUSE_STC"]))
      po["__WXLIB_STC_p"] = addLib(po["LIB_BASENAME_MSW"] + "_stc");
  }

  // Configure "html" library link libs (multilib.bkl#150, depends: base, core, $(EXTRALIBS_HTML))
  void process_html(Options& po, BuildFileOptions& cfg)
  {
    if (cfg["MONOLITHIC"] == "0" && cfg["USE_GUI"] == "1" && cfg["USE_HTML"] == "1")
      po["__WXLIB_HTML_p"] = addLib(po["LIB_BASENAME_MSW"] + "_html");
  }

  // Configure "webview" library link libs (multilib.bkl#180, depends: base, core)
  // TODO: Parse webview from BuildFileOptions
  void process_webview(Options& po, BuildFileOptions& cfg)
  {
    if (cfg["MONOLITHIC"] == "0" && cfg["USE_GUI"] == "1" && cfg["USE_WEBVIEW"] == "1")
      po["__WXLIB_WEBVIEW_p"] = addLib(po["LIB_BASENAME_MSW"] + "_webview");
  }
  
  // Configure "media" library link libs (multilib.bkl#124, depends: base, core, $(EXTRALIBS_MEDIA))
  void process_media(Options& po, BuildFileOptions& cfg)
  {
    if (cfg["MONOLITHIC"] == "0" && cfg["USE_GUI"] == "1" && cfg["USE_MEDIA"] == "1")
      po["__WXLIB_MEDIA_p"] = addLib(po["LIB_BASENAME_MSW"] + "_media");
  }

  // Configure "gl" library link libs (opengl.bkl#17, depends: base, core, $(EXTRALIBS_OPENGL))
  void process_opengl(Options& po, BuildFileOptions& cfg)
  {
    /// Doesn't matter if it's monolithic or not
    if (cfg["USE_OPENGL"] == "1" && cfg["USE_GUI"] == "1")
    {
      po["__WXLIB_OPENGL_p"]  = addLib(po["LIB_BASENAME_MSW"] + "_gl");
      po["EXTRALIBS_FOR_OPENGL"] += addLib("opengl32") + addLib("glu32");
    }
  }

  void process_3(Options& po, const CmdLineOptions& cl, BuildFileOptions& cfg)
  {
    SetupHOptions sho(po["wxcfgsetuphfile"]);
    
    // FIXME: proper place of this would be in a first hook, say process_1();
    if (cl.keyExists("--define-variable"))
    {   
      std::string strDef = cl.keyValue("--define-variable");
      size_t sep = strDef.find("=");
      if (sep != std::string::npos)
      {
        std::string key = strDef.substr(0, sep);
        std::string val = strDef.substr(sep+1, strDef.size()-sep-1);
        
        po[key] = val;
        cfg[key] = val;
        
        if (val == "1" || val == "true")
          sho[key] = true;
        else if (val == "0" || val == "false")
          sho[key] = false;
      }
      else
      {
        std::cout << g_tokError << "Failed to define a variable as '" 
                  << cl.keyValue("--define-variable") << "'." << std::endl;
        std::cout << "The syntax is --define-variable=VARIABLENAME=VARIABLEVALUE" << std::endl;
        exit(1);
      }
    }
        
    /// Overriding flags sho->cfg!!
    /// This makes sho variables haves more privilege than cfg ones
    //-------------------------------------------------------------
    if (sho.keyExists("wxUSE_UNICODE_MSLU"))
      sho["wxUSE_UNICODE_MSLU"] ? cfg["MSLU"] = "1" : cfg["MSLU"] = "0";
    
    // TODO: probably better!!!:
    if (cfg.keyExists("MSLU"))
      sho["wxUSE_UNICODE_MSLU"] ? cfg["MSLU"] = "1" : cfg["MSLU"] = "0";
    
    //-------------------------------------------------------------
    
    /// Overriding compiler programs
    if (cfg.keyExists("CC"))
      m_programs.cc = cfg["CC"];
    
    if (cfg.keyExists("CXX"))
      m_programs.cxx = cfg["CXX"];
    
    if (cfg.keyExists("LD"))
      m_programs.ld = cfg["LD"];
    
    if (cfg.keyExists("LIB"))
      m_programs.lib = cfg["LIB"];
    
    if (cfg.keyExists("WINDRES"))
      m_programs.windres = cfg["WINDRES"];
    
    //-------------------------------------------------------------
    
    
    // BASENAME variables
    po["LIB_BASENAME_MSW"]  = "wx" + po["PORTNAME"] + po["WXUNIVNAME"] + po["WX_RELEASE_NODOT"];
    po["LIB_BASENAME_MSW"] += po["WXUNICODEFLAG"] + po["WXDEBUGFLAG"] + cfg["WX_LIB_FLAVOUR"];
    
    po["LIB_BASENAME_BASE"]  = "wxbase" + po["WX_RELEASE_NODOT"] + po["WXUNICODEFLAG"];
    po["LIB_BASENAME_BASE"] += po["WXDEBUGFLAG"] + cfg["WX_LIB_FLAVOUR"];
    
    // The requested libraries are passed in at the command line. There are two 'magic' cases,
    // "std" indicates that we want the standard libraries, which are defined in stdLibs. In
    // this case we should replace "std" with stdLibs.
    // The second 'magic' case is "all", which requests all of the installed libraries. We need
    // to fetch these manually, as we don't know, a priori, what was built (this is done when
    // wx-config.in is made into wx-config on Unix-like builds)
    
    std::vector<std::string> cl_libs = cl.getLibs();
    
    if (contains(cl_libs, "std"))
      cl_libs = replace(cl_libs, "std", stdLibs);
    else if (contains(cl_libs, "all"))
      cl_libs = replace(cl_libs, "all", getInstalledLibs(po));

    // Now generate the main library outputs
    std::vector<std::string>::iterator i;
    for (i = cl_libs.begin(); i != cl_libs.end(); ++i)
    {
      std::string lib = *i;
      
      if (lib == "base")
        process_base(po, cfg);
      else if (lib == "net")
        process_net(po, cfg);
      else if (lib == "xml")
        process_xml(po, cfg);
      else if (lib == "core")
        process_core(po,cfg);
      else if (lib == "adv")
        process_adv(po, cfg);
      else if (lib == "qa")
        process_qa(po, cfg);
      else if (lib == "html")
        process_html(po, cfg);
      else if (lib == "media")
        process_media(po, cfg);
      else if (lib == "gl")
        process_opengl(po, cfg);
      else if (lib == "aui")
        process_aui(po, cfg, sho);
      else if (lib == "propgrid")
        process_propgrid(po, cfg, sho);
      else if (lib == "ribbon")
        process_ribbon(po, cfg, sho);
      else if (lib == "richtext")
        process_richtext(po, cfg, sho);
      else if (lib == "webview")
        process_webview(po, cfg);
      else if (lib == "stc")
        process_stc(po, cfg, sho);
      else if (lib == "xrc")
        process_xrc(po, cfg, sho);
      else
        po["__WXLIB_ARGS_p"] += addLib(po["LIB_BASENAME_MSW"] + "_" + lib);
    }
    
    if (cfg["MONOLITHIC"] == "1")
      po["__WXLIB_MONO_p"]  = addLib(po["LIB_BASENAME_MSW"]);
    
    
    /// External libs (to wxWidgets)
    
    if (cfg["USE_GUI"] == "1" && sho["wxUSE_LIBTIFF"])
      po["__LIB_TIFF_p"] = addLib("wxtiff" + po["WXDEBUGFLAG"]);
    
    if (cfg["USE_GUI"] == "1" && sho["wxUSE_LIBJPEG"])
      po["__LIB_JPEG_p"] = addLib("wxjpeg" + po["WXDEBUGFLAG"]);
    
    if (cfg["USE_GUI"] == "1" && sho["wxUSE_LIBPNG"] && sho["wxUSE_ZLIB"])
      po["__LIB_PNG_p"] = addLib("wxpng" + po["WXDEBUGFLAG"]);
    
    if (sho["wxUSE_ZLIB"])
      po["__LIB_ZLIB_p"] = addLib("wxzlib" + po["WXDEBUGFLAG"]);
    
    if (sho["wxUSE_REGEX"])
      po["__LIB_REGEX_p"] = addLib("wxregex" + po["WXUNICODEFLAG"] + po["WXDEBUGFLAG"]);
    
    // FIXME: in truth the check should be for wxUSE_XML but... the sho parser is very simple :P
    if (sho["wxUSE_XRC"])
      po["__LIB_EXPAT_p"] = addLib("wxexpat" + po["WXDEBUGFLAG"]);
            
    if (cfg["MSLU"] == "1")
      po["__LIB_UNICOWS_p"] = addLib("unicows");
    
    if (cfg["USE_GDIPLUS"] == "1")
      po["__GDIPLUS_LIB_p"] = addLib("gdiplus");
    
    po["__LIB_KERNEL32_p"] = addLib("kernel32");        
    po["__LIB_USER32_p"] = addLib("user32");
    po["__LIB_GDI32_p"] = addLib("gdi32");
    po["__LIB_COMDLG32_p"] = addLib("comdlg32");
    po["__LIB_WINSPOOL_p"] = addLib("winspool");
    po["__LIB_WINMM_p"] = addLib("winmm");
    po["__LIB_SHELL32_p"] = addLib("shell32");
    po["__LIB_COMCTL32_p"] = addLib("comctl32");
    
    if (sho["wxUSE_OLE"])
    {
      po["__LIB_OLE32_p"] = addLib("ole32");
      po["__LIB_OLEAUT32_p"] = addLib("oleaut32");
      po["__LIB_OLEACC_p"] = addLib("oleacc");    // NOTE: not being used
      po["__LIB_OLE2W32_p"] = addLib("ole2w32");  // NOTE: not being used
    }
    
    po["__LIB_UUID_p"] = addLib("uuid");
    po["__LIB_RPCRT4_p"] = addLib("rpcrt4");
    po["__LIB_ADVAPI32_p"] = addLib("advapi32");
    
    if (sho["wxUSE_SOCKETS"])
      po["__LIB_WSOCK32_p"] = addLib("wsock32");
    
    
    /*  TODO: From BAKEFILE
        <!-- link-in system libs that wx depends on: -->
        <!-- If on borland, we don't need to do much            -->
        <if cond="FORMAT=='borland'">
            <sys-lib>ole2w32</sys-lib>
            <sys-lib>odbc32</sys-lib>
        </if>

        <!-- Non-borland, on the other hand...                  -->
        <if cond="FORMAT not in ['borland','msevc4prj']">
            <sys-lib>kernel32</sys-lib>
            <sys-lib>user32</sys-lib>
            <sys-lib>gdi32</sys-lib>
            <sys-lib>comdlg32</sys-lib>
            <sys-lib>winspool</sys-lib>
            <sys-lib>winmm</sys-lib>
            <sys-lib>shell32</sys-lib>
            <sys-lib>comctl32</sys-lib>
            <sys-lib>ole32</sys-lib>
            <sys-lib>oleaut32</sys-lib>
            <sys-lib>uuid</sys-lib>
            <sys-lib>rpcrt4</sys-lib>
            <sys-lib>advapi32</sys-lib>
            <sys-lib>wsock32</sys-lib>
            <sys-lib>odbc32</sys-lib>
        </if>

        <!-- Libs common to both borland and MSVC               -->
        <if cond="FORMAT=='msvc' or FORMAT=='msvc6prj' or FORMAT=='borland'">
            <sys-lib>oleacc</sys-lib>
*/     
  }
    
  std::string getAllLibs(Options& po)
  {
    std::string libs;
    
    // __WXLIB_ARGS_p is a library supplied by the user. Put these first as probably depend on others
    libs += po["__WXLIB_ARGS_p"];
    
    // Main list of DLLs: ordered by: xrc stc richtext ribbon propgrid webview aui gl media html qa adv core xml net base
    libs += po["__WXLIB_XRC_p"]    + po["__WXLIB_STC_p"]      + po["__WXLIB_RICHTEXT_p"];
    libs += po["__WXLIB_RIBBON_p"] + po["__WXLIB_PROPGRID_p"] + po["__WXLIB_WEBVIEW_p"];
    libs += po["__WXLIB_AUI_p"]    + po["__WXLIB_OPENGL_p"]   + po["__WXLIB_MEDIA_p"];
    libs += po["__WXLIB_HTML_p"]   + po["__WXLIB_QA_p"]       + po["__WXLIB_ADV_p"];
    libs += po["__WXLIB_CORE_p"]   + po["__WXLIB_XML_p"]      + po["__WXLIB_NET_p"];
    libs += po["__WXLIB_BASE_p"]   + po["__WXLIB_MONO_p"];
    
    libs += po["__LIB_TIFF_p"] + po["__LIB_JPEG_p"] + po["__LIB_PNG_p"];
    libs += po["__LIB_ZLIB_p"] + po["__LIB_REGEX_p"] + po["__LIB_EXPAT_p"];
    libs += po["EXTRALIBS_FOR_OPENGL"];
    libs += po["EXTRALIBS_FOR_BASE"] + po["__UNICOWS_LIB_p"] + po["__GDIPLUS_LIB_p"];
    libs += po["__LIB_KERNEL32_p"] + po["__LIB_USER32_p"] + po["__LIB_GDI32_p"];
    libs += po["__LIB_COMDLG32_p"] + po["__LIB_REGEX_p"] + po["__LIB_WINSPOOL_p"];
    libs += po["__LIB_WINMM_p"] + po["__LIB_SHELL32_p"] + po["__LIB_COMCTL32_p"];
    libs += po["__LIB_OLE32_p"] + po["__LIB_OLEAUT32_p"] + po["__LIB_UUID_p"];
    libs += po["__LIB_RPCRT4_p"] + po["__LIB_ADVAPI32_p"] + po["__LIB_WSOCK32_p"];
    
    return libs;
  }

  void getVariablesValues(Options& po, const CmdLineOptions& cl, BuildFileOptions& cfg)
  {
    if (cl.keyExists("--variable"))
    {
      std::string var = cl.keyValue("--variable");
      
      if (po.keyExists(var))
        po["variable"] += "PO: " + var + "=" + po[var] + "\n";
      else
        po["variable"] += "PO: " + var + " does not exist.\n";
      
      if (cfg.keyExists(var))
        po["variable"] += "CFG: " + var + "=" + cfg[var] + "\n";
      else
        po["variable"] += "CFG: " + var + " does not exist.\n";
    }
  }
  
protected:
  // set the following members in your class
  std::string m_name;
  CompilerPrograms m_programs;
  CompilerSwitches m_switches;
};

// -------------------------------------------------------------------------------------------------

/// MinGW compiler
class CompilerMinGW : public Compiler
{
public:
  CompilerMinGW() : Compiler("gcc")
  {
    m_programs.cc = "mingw32-gcc";
    m_programs.cxx = "mingw32-g++";
    m_programs.ld = "mingw32-g++ -shared -fPIC -o ";
    m_programs.lib = "ar.exe";
    m_programs.windres = "windres";
    
    m_switches.includeDirs = "-I";
    m_switches.resIncludeDirs = "--include-dir";
    m_switches.libDirs = "-L";
    m_switches.linkLibs = "-l";
    m_switches.libPrefix = "lib";
    m_switches.libExtension = "a";
    m_switches.defines = "-D";
    m_switches.resDefines = "--define";
    m_switches.genericSwitch = "-";
    m_switches.forceCompilerUseQuotes = false;
    m_switches.forceLinkerUseQuotes = false;
    m_switches.linkerNeedsLibPrefix = false;
    m_switches.linkerNeedsLibExtension = false;
    m_switches.supportsPCH = true;
    m_switches.PCHExtension = "h.gch";
  }
  
  void process(Options& po, const CmdLineOptions& cl)
  {
    /// Searchs for '<prefix>\build\msw\config.*' first
    std::string cfg_first = po["prefix"] + "\\build\\msw\\config." + getName();
    
    /// config.* options
    BuildFileOptions cfg(cfg_first);
    
    /// build.cfg options
    cfg.parse(po["wxcfgfile"]);
    
    
    // ### Variables: ###
    po["WX_RELEASE_NODOT"] = cfg["WXVER_MAJOR"] + cfg["WXVER_MINOR"];
    if (po["WX_RELEASE_NODOT"].empty())
      po["WX_RELEASE_NODOT"] = "29";
    
    // ### Conditionally set variables: ###
    if (cfg["GCC_VERSION"] == "2.95")
      po["GCCFLAGS"] = addFlag("-fvtable-thunks");
    
    if (cfg["USE_GUI"] == "0")
      po["PORTNAME"] = "base";
    
    if (cfg["USE_GUI"] == "1")
      po["PORTNAME"] = "msw";
    
    if (cfg["BUILD"] == "debug" && cfg["DEBUG_FLAG"] == "default")
      po["WXDEBUGFLAG"] = "";
    
    if (cfg["DEBUG_FLAG"] == "1")
      po["WXDEBUGFLAG"] = "";
    
    if (cfg["UNICODE"] == "1")
      po["WXUNICODEFLAG"] = "u";
    
    if (cfg["WXUNIV"] == "1")
      po["WXUNIVNAME"] = "univ";
    
    if (cfg["SHARED"] == "1")
      {
        po["WXDLLFLAG"] = "dll";
        po["LIBTYPE_SUFFIX"] = "dll";
      }
    
    if (cfg["SHARED"] == "0")
      po["LIBTYPE_SUFFIX"] = "lib";
    
    if (cfg["MONOLITHIC"] == "0")
      po["EXTRALIBS_FOR_BASE"] = "";
    
    if (cfg["MONOLITHIC"] == "1")
      po["EXTRALIBS_FOR_BASE"] = "";
    
    if (cfg["BUILD"] == "debug")
      po["__OPTIMIZEFLAG_2"] = addFlag("-O0");
    
    if (cfg["BUILD"] == "release")
      po["__OPTIMIZEFLAG_2"] = addFlag("-O2");
    
    if (cfg["USE_RTTI"] == "1")
      po["__RTTIFLAG_5"] = addFlag("");
    
    if (cfg["USE_EXCEPTIONS"] == "0")
      po["__EXCEPTIONSFLAG_6"] = addFlag("-fno-exceptions");
    
    if (cfg["USE_EXCEPTIONS"] == "1")
      po["__EXCEPTIONSFLAG_6"] = addFlag("");
    
    if (cfg["WXUNIV"] == "1")
      po["__WXUNIV_DEFINE_p"] = addDefine("__WXUNIVERSAL__");
    
    if (cfg["WXUNIV"] == "1")
      po["__WXUNIV_DEFINE_p_1"] = addResDefine("__WXUNIVERSAL__");
    
    if (cfg["BUILD"] == "debug" && cfg["DEBUG_FLAG"] == "default")
      po["__DEBUG_DEFINE_p"] = addDefine("__WXDEBUG__");
    
    if (cfg["DEBUG_FLAG"] == "1")
      po["__DEBUG_DEFINE_p"] = addDefine("__WXDEBUG__");
    
    if (cfg["BUILD"] == "debug" && cfg["DEBUG_FLAG"] == "default")
      po["__DEBUG_DEFINE_p_1"] = addResDefine("__WXDEBUG__");
    
    if (cfg["DEBUG_FLAG"] == "1")
      po["__DEBUG_DEFINE_p_1"] = addResDefine("__WXDEBUG__");
    
    if (cfg["USE_EXCEPTIONS"] == "0")
      po["__EXCEPTIONS_DEFINE_p"] = addDefine("wxNO_EXCEPTIONS");
    
    if (cfg["USE_EXCEPTIONS"] == "0")
      po["__EXCEPTIONS_DEFINE_p_1"] = addResDefine("wxNO_EXCEPTIONS");
    
    if (cfg["USE_RTTI"] == "0")
      po["__RTTI_DEFINE_p"] = addDefine("wxNO_RTTI");
    
    if (cfg["USE_RTTI"] == "0")
      po["__RTTI_DEFINE_p_1"] = addResDefine("wxNO_RTTI");
    
    if (cfg["USE_THREADS"] == "0")
      po["__THREAD_DEFINE_p"] = addDefine("wxNO_THREADS");
    
    if (cfg["USE_THREADS"] == "0")
      po["__THREAD_DEFINE_p_1"] = addResDefine("wxNO_THREADS");
    
    if (cfg["UNICODE"] == "1")
      po["__UNICODE_DEFINE_p"] = addDefine("_UNICODE");
    
    if (cfg["UNICODE"] == "1")
      po["__UNICODE_DEFINE_p_1"] = addResDefine("_UNICODE");
    
    if (cfg["MSLU"] == "1")
      po["__MSLU_DEFINE_p"] = addDefine("wxUSE_UNICODE_MSLU=1");
    
    if (cfg["MSLU"] == "1")
      po["__MSLU_DEFINE_p_1"] = addResDefine("wxUSE_UNICODE_MSLU=1");
    
    if (cfg["USE_GDIPLUS"] == "1")
      po["__GFXCTX_DEFINE_p"] = addDefine("wxUSE_GRAPHICS_CONTEXT=1");
    
    if (cfg["USE_GDIPLUS"] == "1")
      po["__GFXCTX_DEFINE_p_1"] = addResDefine("wxUSE_GRAPHICS_CONTEXT=1");
    
    if (cfg["SHARED"] == "1")
      po["__DLLFLAG_p"] = addDefine("WXUSINGDLL");
    
    if (cfg["SHARED"] == "1")
      po["__DLLFLAG_p_1"] = addResDefine("WXUSINGDLL");
    
    process_3(po, cl, cfg);
    //----------------------------------------------------
    
    if (cfg["BUILD"] == "debug" && cfg["DEBUG_INFO"] == "default")
      po["__DEBUGINFO"] = addFlag("-g");
    
    if (cfg["BUILD"] == "release" && cfg["DEBUG_INFO"] == "default")
      po["__DEBUGINFO"] = addFlag("");
    
    if (cfg["DEBUG_INFO"] == "0")
      po["__DEBUGINFO"] = addFlag("");
    
    if (cfg["DEBUG_INFO"] == "1")
      po["__DEBUGINFO"] = addFlag("-g");
    
    if (cfg["USE_THREADS"] == "0")
      po["__THREADSFLAG"] = addFlag("");
    
    if (cfg["USE_THREADS"] == "1")
      po["__THREADSFLAG"] = addFlag("-mthreads");
    
    //----------------------------------------------------
    
    // ### Variables, Part 2: ###
    po["LIBDIRNAME"] = po["prefix"] + "\\lib\\" + getName() + "_" + po["LIBTYPE_SUFFIX"] + cfg["CFG"];
    
    po["SETUPHDIR"]  = po["LIBDIRNAME"] + "\\" + po["PORTNAME"] + po["WXUNIVNAME"];
    po["SETUPHDIR"] += po["WXUNICODEFLAG"] + po["WXDEBUGFLAG"];
    
    po["cflags"]  = easyMode(po["__DEBUGINFO"]) + easyMode(po["__OPTIMIZEFLAG_2"]) + po["__THREADSFLAG"];
    po["cflags"] += po["GCCFLAGS"] + addDefine("HAVE_W32API_H") + addDefine("__WXMSW__") + po["__WXUNIV_DEFINE_p"];
    po["cflags"] += po["__DEBUG_DEFINE_p"] + po["__EXCEPTIONS_DEFINE_p"] + po["__RTTI_DEFINE_p"];
    po["cflags"] += po["__THREAD_DEFINE_p"] + po["__UNICODE_DEFINE_p"] + po["__MSLU_DEFINE_p"];
    po["cflags"] += po["__GFXCTX_DEFINE_p"];
    po["cflags"] += addIncludeDir(po["SETUPHDIR"]) + addIncludeDir(po["prefix"] + "\\include");/*-W */
    po["cflags"] += easyMode(addFlag("-Wall")) + easyMode(addIncludeDir(".")) + po["__DLLFLAG_p"];
    po["cflags"] += easyMode(addIncludeDir(".\\..\\..\\samples")) + /*addDefine("NOPCH") +*/ po["__RTTIFLAG_5"] + po["__EXCEPTIONSFLAG_6"];
    po["cflags"] += addFlag("-Wno-ctor-dtor-privacy") + addFlag("-pipe") + addFlag("-fmessage-length=0");
    po["cflags"] += addFlag("-Wl,--subsystem,windows") + addFlag("-mwindows") + addFlag("-Wl,--enable-auto-import");
    po["cflags"] += cfg["CPPFLAGS"] + " " + cfg["CXXFLAGS"] + " ";
    
    po["libs"]  = cfg["LDFLAGS"] + " ";
    po["libs"] += easyMode(po["__DEBUGINFO"]) + po["__THREADSFLAG"];
    po["libs"] += addLinkerDir(po["LIBDIRNAME"]);
    po["libs"] += easyMode(addFlag("-Wl,--subsystem,windows")) + easyMode(addFlag("-mwindows"));
    po["libs"] += getAllLibs(po);
    
    po["rcflags"]  = addFlag("--use-temp-file") + addResDefine("__WXMSW__") + po["__WXUNIV_DEFINE_p_1"];
    po["rcflags"] += po["__DEBUG_DEFINE_p_1"] + po["__EXCEPTIONS_DEFINE_p_1"];
    po["rcflags"] += po["__RTTI_DEFINE_p_1"] + po["__THREAD_DEFINE_p_1"] + po["__UNICODE_DEFINE_p_1"];
    po["rcflags"] += po["__MSLU_DEFINE_p_1"] + po["__GFXCTX_DEFINE_p_1"] + addResIncludeDir(po["SETUPHDIR"]);
    po["rcflags"] += addResIncludeDir(po["prefix"] + "\\include") + easyMode(addResIncludeDir("."));
    po["rcflags"] += po["__DLLFLAG_p_1"];
    po["rcflags"] += easyMode(addResIncludeDir(po["prefix"] + "\\samples"));
    
    po["release"] = cfg["WXVER_MAJOR"] + "." + cfg["WXVER_MINOR"];
    po["version"] = cfg["WXVER_MAJOR"] + "." + cfg["WXVER_MINOR"] + "." + cfg["WXVER_RELEASE"];
    po["basename"]  = "wx" + po["PORTNAME"] + po["WXUNIVNAME"] + po["WX_RELEASE_NODOT"];
    po["basename"] += po["WXUNICODEFLAG"] + po["WXDEBUGFLAG"] + cfg["WX_LIB_FLAVOUR"];
    po["cc"] = m_programs.cc;
    po["cxx"] = m_programs.cxx;
    po["ld"] = m_programs.ld;
    
    getVariablesValues(po, cl, cfg);
  }
};

// -------------------------------------------------------------------------------------------------

/// DMars compiler
class CompilerDMC : public Compiler
{
public:
    CompilerDMC() : Compiler("dmc")
  {
    m_programs.cc = "dmc";
    m_programs.cxx = "dmc";
    m_programs.ld = "link";
    m_programs.lib = "lib";
    m_programs.windres = "rcc";
    
    m_switches.includeDirs = "-I";
    m_switches.resIncludeDirs = "--include-dir"; ////////////
    m_switches.libDirs = "";
    m_switches.linkLibs = "";
    m_switches.libPrefix = "";
    m_switches.libExtension = "lib";
    m_switches.defines = "-D";
    m_switches.resDefines = "-D";//////////////////////////////////(doesnt uses space)
    m_switches.genericSwitch = "-";
    m_switches.forceCompilerUseQuotes = false;
    m_switches.forceLinkerUseQuotes = true;
    m_switches.linkerNeedsLibPrefix = false;
    m_switches.linkerNeedsLibExtension = true;
  }
  
  
  void process(Options& po, const CmdLineOptions& cl)
  {
    /// Searchs for '<prefix>\build\msw\config.*' first
    std::string cfg_first = po["prefix"] + "\\build\\msw\\config." + getName();
    
    /// config.* options
    BuildFileOptions cfg(cfg_first);
    
    /// build.cfg options
    cfg.parse(po["wxcfgfile"]);
    
    
    // ### Variables: ###
    po["WX_RELEASE_NODOT"] = cfg["WXVER_MAJOR"] + cfg["WXVER_MINOR"];
    if (po["WX_RELEASE_NODOT"].empty())
      po["WX_RELEASE_NODOT"] = "26";
    
    // ### Conditionally set variables: ###
    if (cfg["USE_GUI"] == "0")
      po["PORTNAME"] = "base";
    
    if (cfg["USE_GUI"] == "1")
      po["PORTNAME"] = "msw";
    
    if (cfg["BUILD"] == "debug" && cfg["DEBUG_FLAG"] == "default")
      po["WXDEBUGFLAG"] = "d";
    
    if (cfg["DEBUG_FLAG"] == "1")
      po["WXDEBUGFLAG"] = "d";
    
    if (cfg["UNICODE"] == "1")
      po["WXUNICODEFLAG"] = "u";
    
    if (cfg["WXUNIV"] == "1")
      po["WXUNIVNAME"] = "univ";
    
    if (cfg["SHARED"] == "1")
      po["WXDLLFLAG"] = "dll";
    
    if (cfg["SHARED"] == "0")
      po["LIBTYPE_SUFFIX"] = "lib";
    
    if (cfg["SHARED"] == "1")
      po["LIBTYPE_SUFFIX"] = "dll";
    
    if (cfg["MONOLITHIC"] == "0")
      po["EXTRALIBS_FOR_BASE"] = "";
    
    if (cfg["MONOLITHIC"] == "1")
      po["EXTRALIBS_FOR_BASE"] = "";
    
    //----------------------------------------------------
    
    if (cfg["BUILD"] == "debug" && cfg["DEBUG_INFO"] == "default")
      po["__DEBUGINFO_0"] = addFlag("-g");
    
    if (cfg["BUILD"] == "release" && cfg["DEBUG_INFO"] == "default")
      po["__DEBUGINFO_0"] = addFlag("");
    
    if (cfg["DEBUG_INFO"] == "0")
      po["__DEBUGINFO_0"] = addFlag("");
    
    if (cfg["DEBUG_INFO"] == "1")
      po["__DEBUGINFO_0"] = addFlag("-g");
    
    if (cfg["BUILD"] == "debug" && cfg["DEBUG_INFO"] == "default")
      po["__DEBUGINFO_1"] = addFlag("/DEBUG /CODEVIEW");
    
    if (cfg["BUILD"] == "release" && cfg["DEBUG_INFO"] == "default")
      po["__DEBUGINFO_1"] = addFlag("");
    
    if (cfg["DEBUG_INFO"] == "0")
      po["__DEBUGINFO_1"] = addFlag("");
    
    if (cfg["DEBUG_INFO"] == "1")
      po["__DEBUGINFO_1"] = addFlag("/DEBUG /CODEVIEW");
    /*
      if (cfg["BUILD"] == "debug" && cfg["DEBUG_RUNTIME_LIBS"] == "default")
      po["____DEBUGRUNTIME_2_p"] = addDefine("_DEBUG");
      
      if (cfg["BUILD"] == "release" && cfg["DEBUG_RUNTIME_LIBS"] == "default")
      po["____DEBUGRUNTIME_2_p"] = addDefine("");
      
      if (cfg["DEBUG_RUNTIME_LIBS"] == "0")
      po["____DEBUGRUNTIME_2_p"] = addDefine("");
      
      if (cfg["DEBUG_RUNTIME_LIBS"] == "1")
      po["____DEBUGRUNTIME_2_p"] = addDefine("_DEBUG");
      
      if (cfg["BUILD"] == "debug" && cfg["DEBUG_RUNTIME_LIBS"] == "default")
      po["____DEBUGRUNTIME_2_p_1"] = addResDefine("_DEBUG");
      
      if (cfg["BUILD"] == "release" && cfg["DEBUG_RUNTIME_LIBS"] == "default")
      po["____DEBUGRUNTIME_2_p_1"] = addResDefine("");
      
      if (cfg["DEBUG_RUNTIME_LIBS"] == "0")
      po["____DEBUGRUNTIME_2_p_1"] = addResDefine("");
      
      if (cfg["DEBUG_RUNTIME_LIBS"] == "1")
      po["____DEBUGRUNTIME_2_p_1"] = addResDefine("_DEBUG");
      
      if (cfg["BUILD"] == "debug" && cfg["DEBUG_RUNTIME_LIBS"] == "default")
      po["__DEBUGRUNTIME_3"] = "d";
      
      if (cfg["BUILD"] == "release" && cfg["DEBUG_RUNTIME_LIBS"] == "default")
      po["__DEBUGRUNTIME_3"] = "";
      
      if (cfg["DEBUG_RUNTIME_LIBS"] == "0")
      po["__DEBUGRUNTIME_3"] = "";
      
      if (cfg["DEBUG_RUNTIME_LIBS"] == "1")
      po["__DEBUGRUNTIME_3"] = "d";
    */
    //----------------------------------------------------
    
    if (cfg["BUILD"] == "debug")
      po["__OPTIMIZEFLAG_4"] = addFlag("-o+none");//2
    
    if (cfg["BUILD"] == "release")
      po["__OPTIMIZEFLAG_4"] = addFlag("-o");//2
    /*
      if (cfg["USE_THREADS"] == "0")
      po["__THREADSFLAG_7"] = "L";
      
      if (cfg["USE_THREADS"] == "1")
      po["__THREADSFLAG_7"] = "T";
    */
    if (cfg["RUNTIME_LIBS"] == "dynamic")
      po["__RUNTIME_LIBS_8"] = "-ND";//5 // TODO: addFlag?
    
    if (cfg["RUNTIME_LIBS"] == "static")
      po["__RUNTIME_LIBS_8"] = "";//5
    
    //----------------------------------------------------
    
    if (cfg["USE_RTTI"] == "0")
      po["__RTTIFLAG_9"] = addFlag("");//6
    
    if (cfg["USE_RTTI"] == "1")
      po["__RTTIFLAG_9"] = addFlag("-Ar");//6
    
    if (cfg["USE_EXCEPTIONS"] == "0")
      po["__EXCEPTIONSFLAG_10"] = addFlag("");//7
    
    if (cfg["USE_EXCEPTIONS"] == "1")
      po["__EXCEPTIONSFLAG_10"] = addFlag("-Ae");//7
    
    //----------------------------------------------------
    /*
      if (cfg["BUILD"] == "debug" && cfg["DEBUG_RUNTIME_LIBS"] == "0")
      po["__NO_VC_CRTDBG_p"] = addDefine("__NO_VC_CRTDBG__");
      
      if (cfg["BUILD"] == "release" && cfg["DEBUG_FLAG"] == "1")
      po["__NO_VC_CRTDBG_p"] = addDefine("__NO_VC_CRTDBG__");
      
      if (cfg["BUILD"] == "debug" && cfg["DEBUG_RUNTIME_LIBS"] == "0")
      po["__NO_VC_CRTDBG_p_1"] = addResDefine("__NO_VC_CRTDBG__");
      
      if (cfg["BUILD"] == "release" && cfg["DEBUG_FLAG"] == "1")
      po["__NO_VC_CRTDBG_p_1"] = addResDefine("__NO_VC_CRTDBG__");
    */
    if (cfg["WXUNIV"] == "1")
      po["__WXUNIV_DEFINE_p"] = addDefine("__WXUNIVERSAL__");
    /*
      if (cfg["WXUNIV"] == "1")
      po["__WXUNIV_DEFINE_p_1"] = addResDefine("__WXUNIVERSAL__");
    */
    if (cfg["BUILD"] == "debug" && cfg["DEBUG_FLAG"] == "default")
      po["__DEBUG_DEFINE_p"] = addDefine("__WXDEBUG__");
    
    if (cfg["DEBUG_FLAG"] == "1")
      po["__DEBUG_DEFINE_p"] = addDefine("__WXDEBUG__");
    /*
      if (cfg["BUILD"] == "debug" && cfg["DEBUG_FLAG"] == "default")
      po["__DEBUG_DEFINE_p_1"] = addResDefine("__WXDEBUG__");
      
      if (cfg["DEBUG_FLAG"] == "1")
      po["__DEBUG_DEFINE_p_1"] = addResDefine("__WXDEBUG__");
    */
    if (cfg["USE_EXCEPTIONS"] == "0")
      po["__EXCEPTIONS_DEFINE_p"] = addDefine("wxNO_EXCEPTIONS");
    /*
      if (cfg["USE_EXCEPTIONS"] == "0")
      po["__EXCEPTIONS_DEFINE_p_1"] = addResDefine("wxNO_EXCEPTIONS");
    */
    if (cfg["USE_RTTI"] == "0")
      po["__RTTI_DEFINE_p"] = addDefine("wxNO_RTTI");
    /*
      if (cfg["USE_RTTI"] == "0")
      po["__RTTI_DEFINE_p_1"] = addResDefine("wxNO_RTTI");
    */
    if (cfg["USE_THREADS"] == "0")
      po["__THREAD_DEFINE_p"] = addDefine("wxNO_THREADS");
    /*
      if (cfg["USE_THREADS"] == "0")
      po["__THREAD_DEFINE_p_1"] = addResDefine("wxNO_THREADS");
    */
    if (cfg["UNICODE"] == "1")
      po["__UNICODE_DEFINE_p"] = addDefine("_UNICODE");
    /*
      if (cfg["UNICODE"] == "1")
      po["__UNICODE_DEFINE_p_1"] = addResDefine("_UNICODE");
    */
    if (cfg["MSLU"] == "1")
      po["__MSLU_DEFINE_p"] = addDefine("wxUSE_UNICODE_MSLU=1");
    /*
      if (cfg["MSLU"] == "1")
      po["__MSLU_DEFINE_p_1"] = addResDefine("wxUSE_UNICODE_MSLU=1");
    */            
    if (cfg["USE_GDIPLUS"] == "1")
      po["__GFXCTX_DEFINE_p"] = addDefine("wxUSE_GRAPHICS_CONTEXT=1");
    /*
      if (cfg["USE_GDIPLUS"] == "1")
      po["__GFXCTX_DEFINE_p_1"] = addResDefine("wxUSE_GRAPHICS_CONTEXT=1");            
    */
    if (cfg["SHARED"] == "1")
      po["__DLLFLAG_p"] = addDefine("WXUSINGDLL");
    /*
      if (cfg["SHARED"] == "1")
      po["__DLLFLAG_p_1"] = addResDefine("WXUSINGDLL");
    */
    process_3(po, cl, cfg);
    
    
    // ### Variables, Part 2: ###
    po["LIBDIRNAME"] = po["prefix"] + "\\lib\\" + getName() + "_" + po["LIBTYPE_SUFFIX"] + cfg["CFG"];
    
    po["SETUPHDIR"]  = po["LIBDIRNAME"] + "\\" + po["PORTNAME"] + po["WXUNIVNAME"];
    po["SETUPHDIR"] += po["WXUNICODEFLAG"] + po["WXDEBUGFLAG"];
    
    po["cflags"]  = easyMode(po["__DEBUGINFO_0"]) + easyMode(po["__OPTIMIZEFLAG_4"]);
    po["cflags"] += po["__RUNTIME_LIBS_8"] + " " + addDefine("_WIN32_WINNT=0x0400");
    po["cflags"] += addDefine("__WXMSW__") + po["__WXUNIV_DEFINE_p"];
    po["cflags"] += po["__DEBUG_DEFINE_p"] + po["__EXCEPTIONS_DEFINE_p"] + po["__RTTI_DEFINE_p"];
    po["cflags"] += po["__THREAD_DEFINE_p"] + po["__UNICODE_DEFINE_p"] + po["__MSLU_DEFINE_p"];
    po["cflags"] += po["__GFXCTX_DEFINE_p"];
    po["cflags"] += addIncludeDir(po["SETUPHDIR"]) + addIncludeDir(po["prefix"] + "\\include") + easyMode(addFlag("-w-")) + easyMode(addIncludeDir(".")) + po["__DLLFLAG_p"] + easyMode(addFlag("-WA"));
    po["cflags"] += easyMode(addIncludeDir(po["prefix"] + "\\samples")) + easyMode(addDefine("NOPCH")) + po["__RTTIFLAG_9"] + po["__EXCEPTIONSFLAG_10"];
    po["cflags"] += cfg["CPPFLAGS"] + " " + cfg["CXXFLAGS"] + " ";
    
    
    
    po["libs"]  = easyMode(addFlag("/NOLOGO")) + easyMode(addFlag("/SILENT"));
    po["libs"] += easyMode(addFlag("/NOI")) + easyMode(addFlag("/DELEXECUTABLE"));
    po["libs"] += easyMode(addFlag("/EXETYPE:NT"));
    po["libs"] += cfg["LDFLAGS"] + " ";
    po["libs"] += easyMode(po["__DEBUGINFO_1"]);
    po["libs"] += addLinkerDir(po["LIBDIRNAME"] + "\\");
    po["libs"] += easyMode(addFlag("/su:windows:4.0"));
    po["libs"] += getAllLibs(po);        
    
    po["rcflags"]  = addResDefine("_WIN32_WINNT=0x0400") + addResDefine("__WXMSW__");
    po["rcflags"] += po["__WXUNIV_DEFINE_p"] + po["__DEBUG_DEFINE_p"] + po["__EXCEPTIONS_DEFINE_p"];
    po["rcflags"] += po["__RTTI_DEFINE_p"] + po["__THREAD_DEFINE_p"] + po["__UNICODE_DEFINE_p"];
    po["rcflags"] += po["__MSLU_DEFINE_p"] + po["__GFXCTX_DEFINE_p"] + addResIncludeDir(po["SETUPHDIR"]);
    po["rcflags"] += addResIncludeDir(po["prefix"] + "\\include") + easyMode(addResIncludeDir("."));
    po["rcflags"] += po["__DLLFLAG_p"];
    po["rcflags"] += easyMode(addResIncludeDir(po["prefix"] + "\\samples"));
    po["rcflags"] += easyMode(addFlag("-32")) + easyMode(addFlag("-v-"));
    
    
    po["release"] = cfg["WXVER_MAJOR"] + "." + cfg["WXVER_MINOR"];
    po["version"] = cfg["WXVER_MAJOR"] + "." + cfg["WXVER_MINOR"] + "." + cfg["WXVER_RELEASE"];
    po["basename"]  = "wx" + po["PORTNAME"] + po["WXUNIVNAME"] + po["WX_RELEASE_NODOT"];
    po["basename"] += po["WXUNICODEFLAG"] + po["WXDEBUGFLAG"] + cfg["WX_LIB_FLAVOUR"];
    po["cc"] = m_programs.cc;
    po["cxx"] = m_programs.cxx;
    po["ld"] = m_programs.ld;
    
    getVariablesValues(po, cl, cfg);
  }
  
};

// -------------------------------------------------------------------------------------------------

/// Visual Studio compiler
class CompilerVC : public Compiler
{
public:
  CompilerVC() : Compiler("vc")
  {
    m_programs.cc = "cl";
    m_programs.cxx = "cl";
    m_programs.ld = "link";
    m_programs.lib = "link";
    m_programs.windres = "rc";
    
    m_switches.includeDirs = "/I";
    m_switches.resIncludeDirs = "--include-dir"; ////////////
    m_switches.libDirs = "/LIBPATH:";
    m_switches.linkLibs = "";
    m_switches.libPrefix = "";
    m_switches.libExtension = "lib";
    m_switches.defines = "/D";
    m_switches.resDefines = "/d";
    m_switches.genericSwitch = "/";
    m_switches.forceCompilerUseQuotes = false;
    m_switches.forceLinkerUseQuotes = false;
    m_switches.linkerNeedsLibPrefix = false;
    m_switches.linkerNeedsLibExtension = true;
  }
  
  void process(Options& po, const CmdLineOptions& cl)
  {
    /// Searchs for '<prefix>\build\msw\config.*' first
    std::string cfg_first = po["prefix"] + "\\build\\msw\\config." + getName();
    
    /// config.* options
    BuildFileOptions cfg(cfg_first);
    
    /// build.cfg options
    cfg.parse(po["wxcfgfile"]);
    
    
    // ### Variables: ###
    po["WX_RELEASE_NODOT"] = cfg["WXVER_MAJOR"] + cfg["WXVER_MINOR"];
    if (po["WX_RELEASE_NODOT"].empty())
      po["WX_RELEASE_NODOT"] = "26";
    
    // ### Conditionally set variables: ###
    if (cfg["USE_GUI"] == "0")
      po["PORTNAME"] = "base";
    
    if (cfg["USE_GUI"] == "1")
      po["PORTNAME"] = "msw";
    
    if (cfg["BUILD"] == "debug" && cfg["DEBUG_FLAG"] == "default")
      po["WXDEBUGFLAG"] = "d";
    
    if (cfg["DEBUG_FLAG"] == "1")
      po["WXDEBUGFLAG"] = "d";
    
    if (cfg["UNICODE"] == "1")
      po["WXUNICODEFLAG"] = "u";
    
    if (cfg["WXUNIV"] == "1")
      po["WXUNIVNAME"] = "univ";
    
    if (cfg["SHARED"] == "1")
      po["WXDLLFLAG"] = "dll";
    
    if (cfg["SHARED"] == "0")
      po["LIBTYPE_SUFFIX"] = "lib";
    
    if (cfg["SHARED"] == "1")
      po["LIBTYPE_SUFFIX"] = "dll";
    
    if (cfg["MONOLITHIC"] == "0")
      po["EXTRALIBS_FOR_BASE"] = "";
    
    if (cfg["MONOLITHIC"] == "1")
      po["EXTRALIBS_FOR_BASE"] = "";
    
    if (cfg["TARGET_CPU"] == "amd64")
      po["DIR_SUFFIX_CPU"] = "_amd64";
    
    if (cfg["TARGET_CPU"] == "ia64")
      po["DIR_SUFFIX_CPU"] = "_ia64";
    
    // TODO: TARGET_CPU uses $(CPU), so it will be ignored unless explicitely specified
    if (cfg["TARGET_CPU"] == "$(CPU)")
      po["LINK_TARGET_CPU"] = "";
    
    if (cfg["TARGET_CPU"] == "amd64")
      po["LINK_TARGET_CPU"] = addFlag("/MACHINE:AMD64");
    
    if (cfg["TARGET_CPU"] == "ia64")
      po["LINK_TARGET_CPU"] = addFlag("/MACHINE:IA64");
    
    //----------------------------------------------------
    
    if (cfg["BUILD"] == "debug" && cfg["DEBUG_INFO"] == "default")
      po["__DEBUGINFO_0"] = addFlag("/Zi");
    
    if (cfg["BUILD"] == "release" && cfg["DEBUG_INFO"] == "default")
      po["__DEBUGINFO_0"] = addFlag("");
    
    if (cfg["DEBUG_INFO"] == "0")
      po["__DEBUGINFO_0"] = addFlag("");
    
    if (cfg["DEBUG_INFO"] == "1")
      po["__DEBUGINFO_0"] = addFlag("/Zi");
    
    if (cfg["BUILD"] == "debug" && cfg["DEBUG_INFO"] == "default")
      po["__DEBUGINFO_1"] = addFlag("/DEBUG");
    
    if (cfg["BUILD"] == "release" && cfg["DEBUG_INFO"] == "default")
      po["__DEBUGINFO_1"] = addFlag("");
    
    if (cfg["DEBUG_INFO"] == "0")
      po["__DEBUGINFO_1"] = addFlag("");
    
    if (cfg["DEBUG_INFO"] == "1")
      po["__DEBUGINFO_1"] = addFlag("/DEBUG");
    
    if (cfg["BUILD"] == "debug" && cfg["DEBUG_RUNTIME_LIBS"] == "default")
      po["____DEBUGRUNTIME_2_p"] = addDefine("_DEBUG");
    
    if (cfg["BUILD"] == "release" && cfg["DEBUG_RUNTIME_LIBS"] == "default")
      po["____DEBUGRUNTIME_2_p"] = addDefine("");
    
    if (cfg["DEBUG_RUNTIME_LIBS"] == "0")
      po["____DEBUGRUNTIME_2_p"] = addDefine("");
    
    if (cfg["DEBUG_RUNTIME_LIBS"] == "1")
      po["____DEBUGRUNTIME_2_p"] = addDefine("_DEBUG");
    
    if (cfg["BUILD"] == "debug" && cfg["DEBUG_RUNTIME_LIBS"] == "default")
      po["____DEBUGRUNTIME_2_p_1"] = addResDefine("_DEBUG");
    
    if (cfg["BUILD"] == "release" && cfg["DEBUG_RUNTIME_LIBS"] == "default")
      po["____DEBUGRUNTIME_2_p_1"] = addResDefine("");
    
    if (cfg["DEBUG_RUNTIME_LIBS"] == "0")
      po["____DEBUGRUNTIME_2_p_1"] = addResDefine("");
    
    if (cfg["DEBUG_RUNTIME_LIBS"] == "1")
      po["____DEBUGRUNTIME_2_p_1"] = addResDefine("_DEBUG");
    
    if (cfg["BUILD"] == "debug" && cfg["DEBUG_RUNTIME_LIBS"] == "default")
      po["__DEBUGRUNTIME_3"] = "d";
    
    if (cfg["BUILD"] == "release" && cfg["DEBUG_RUNTIME_LIBS"] == "default")
      po["__DEBUGRUNTIME_3"] = "";
    
    if (cfg["DEBUG_RUNTIME_LIBS"] == "0")
      po["__DEBUGRUNTIME_3"] = "";
    
    if (cfg["DEBUG_RUNTIME_LIBS"] == "1")
      po["__DEBUGRUNTIME_3"] = "d";
    
    //----------------------------------------------------
    
    if (cfg["BUILD"] == "debug")
      po["__OPTIMIZEFLAG_4"] = addFlag("/Od");
    
    if (cfg["BUILD"] == "release")
      po["__OPTIMIZEFLAG_4"] = addFlag("/O2");
    
    if (cfg["USE_THREADS"] == "0")
      po["__THREADSFLAG_7"] = "L";
    
    if (cfg["USE_THREADS"] == "1")
      po["__THREADSFLAG_7"] = "T";
    
    if (cfg["RUNTIME_LIBS"] == "dynamic")
      po["__RUNTIME_LIBS_8"] = "D";
    
    if (cfg["RUNTIME_LIBS"] == "static")
      po["__RUNTIME_LIBS_8"] = po["__THREADSFLAG_7"];
    
    //----------------------------------------------------
    
    if (cfg["USE_RTTI"] == "0")
      po["__RTTIFLAG_9"] = addFlag("");
    
    if (cfg["USE_RTTI"] == "1")
      po["__RTTIFLAG_9"] = addFlag("/GR");
    
    if (cfg["USE_EXCEPTIONS"] == "0")
      po["__EXCEPTIONSFLAG_10"] = addFlag("");
    
    if (cfg["USE_EXCEPTIONS"] == "1")
      po["__EXCEPTIONSFLAG_10"] = addFlag("/EHsc");
    
    //----------------------------------------------------
    
    if (cfg["BUILD"] == "debug" && cfg["DEBUG_RUNTIME_LIBS"] == "0")
      po["__NO_VC_CRTDBG_p"] = addDefine("__NO_VC_CRTDBG__");
    
    if (cfg["BUILD"] == "release" && cfg["DEBUG_FLAG"] == "1")
      po["__NO_VC_CRTDBG_p"] = addDefine("__NO_VC_CRTDBG__");
    
    if (cfg["BUILD"] == "debug" && cfg["DEBUG_RUNTIME_LIBS"] == "0")
      po["__NO_VC_CRTDBG_p_1"] = addResDefine("__NO_VC_CRTDBG__");
    
    if (cfg["BUILD"] == "release" && cfg["DEBUG_FLAG"] == "1")
      po["__NO_VC_CRTDBG_p_1"] = addResDefine("__NO_VC_CRTDBG__");
    
    if (cfg["WXUNIV"] == "1")
      po["__WXUNIV_DEFINE_p"] = addDefine("__WXUNIVERSAL__");
    
    if (cfg["WXUNIV"] == "1")
      po["__WXUNIV_DEFINE_p_1"] = addResDefine("__WXUNIVERSAL__");
    
    if (cfg["BUILD"] == "debug" && cfg["DEBUG_FLAG"] == "default")
      po["__DEBUG_DEFINE_p"] = addDefine("__WXDEBUG__");
    
    if (cfg["DEBUG_FLAG"] == "1")
      po["__DEBUG_DEFINE_p"] = addDefine("__WXDEBUG__");
    
    if (cfg["BUILD"] == "debug" && cfg["DEBUG_FLAG"] == "default")
      po["__DEBUG_DEFINE_p_1"] = addResDefine("__WXDEBUG__");
    
    if (cfg["DEBUG_FLAG"] == "1")
      po["__DEBUG_DEFINE_p_1"] = addResDefine("__WXDEBUG__");
    
    if (cfg["USE_EXCEPTIONS"] == "0")
      po["__EXCEPTIONS_DEFINE_p"] = addDefine("wxNO_EXCEPTIONS");
    
    if (cfg["USE_EXCEPTIONS"] == "0")
      po["__EXCEPTIONS_DEFINE_p_1"] = addResDefine("wxNO_EXCEPTIONS");
    
    if (cfg["USE_RTTI"] == "0")
      po["__RTTI_DEFINE_p"] = addDefine("wxNO_RTTI");
    
    if (cfg["USE_RTTI"] == "0")
      po["__RTTI_DEFINE_p_1"] = addResDefine("wxNO_RTTI");
    
    if (cfg["USE_THREADS"] == "0")
      po["__THREAD_DEFINE_p"] = addDefine("wxNO_THREADS");
    
    if (cfg["USE_THREADS"] == "0")
      po["__THREAD_DEFINE_p_1"] = addResDefine("wxNO_THREADS");
    
    if (cfg["UNICODE"] == "1")
      po["__UNICODE_DEFINE_p"] = addDefine("_UNICODE");
    
    if (cfg["UNICODE"] == "1")
      po["__UNICODE_DEFINE_p_1"] = addResDefine("_UNICODE");
    
    if (cfg["MSLU"] == "1")
      po["__MSLU_DEFINE_p"] = addDefine("wxUSE_UNICODE_MSLU=1");
    
    if (cfg["MSLU"] == "1")
      po["__MSLU_DEFINE_p_1"] = addResDefine("wxUSE_UNICODE_MSLU=1");
    
    if (cfg["USE_GDIPLUS"] == "1")
      po["__GFXCTX_DEFINE_p"] = addDefine("wxUSE_GRAPHICS_CONTEXT=1");
    
    if (cfg["USE_GDIPLUS"] == "1")
      po["__GFXCTX_DEFINE_p_1"] = addResDefine("wxUSE_GRAPHICS_CONTEXT=1");
    
    if (cfg["SHARED"] == "1")
      po["__DLLFLAG_p"] = addDefine("WXUSINGDLL");
    
    if (cfg["SHARED"] == "1")
      po["__DLLFLAG_p_1"] = addResDefine("WXUSINGDLL");
    
    process_3(po, cl, cfg);
    
    
    // ### Variables, Part 2: ###
    po["LIBDIRNAME"] = po["prefix"] + "\\lib\\" + getName() + po["DIR_SUFFIX_CPU"] + "_" + po["LIBTYPE_SUFFIX"] + cfg["CFG"];
    
    po["SETUPHDIR"]  = po["LIBDIRNAME"] + "\\" + po["PORTNAME"] + po["WXUNIVNAME"];
    po["SETUPHDIR"] += po["WXUNICODEFLAG"] + po["WXDEBUGFLAG"];
    
    po["cflags"]  = "/M" + po["__RUNTIME_LIBS_8"] + po["__DEBUGRUNTIME_3"] + " " + addDefine("WIN32");
    po["cflags"] += easyMode(po["__DEBUGINFO_0"]) + easyMode(po["____DEBUGRUNTIME_2_p"]);
    po["cflags"] += easyMode(po["__OPTIMIZEFLAG_4"]) + po["__NO_VC_CRTDBG_p"] + addDefine("__WXMSW__") + po["__WXUNIV_DEFINE_p"];
    po["cflags"] += po["__DEBUG_DEFINE_p"] + po["__EXCEPTIONS_DEFINE_p"] + po["__RTTI_DEFINE_p"];
    po["cflags"] += po["__THREAD_DEFINE_p"] + po["__UNICODE_DEFINE_p"] + po["__MSLU_DEFINE_p"] + po["__GFXCTX_DEFINE_p"];
    po["cflags"] += addIncludeDir(po["SETUPHDIR"]) + addIncludeDir(po["prefix"] + "\\include") + easyMode(addFlag("/W4")) + easyMode(addIncludeDir(".")) + po["__DLLFLAG_p"] + addDefine("_WINDOWS");
    po["cflags"] += easyMode(addIncludeDir(po["prefix"] + "\\samples")) + easyMode(addDefine("NOPCH")) + po["__RTTIFLAG_9"] + po["__EXCEPTIONSFLAG_10"];
    po["cflags"] += cfg["CPPFLAGS"] + " " + cfg["CXXFLAGS"] + " ";
    
    po["libs"]  = easyMode(addFlag("/NOLOGO"));
    po["libs"] += cfg["LDFLAGS"] + " ";
    po["libs"] += easyMode(po["__DEBUGINFO_1"]) + " " + po["LINK_TARGET_CPU"] + " ";
    po["libs"] += addLinkerDir(po["LIBDIRNAME"]);
    po["libs"] += easyMode(addFlag("/SUBSYSTEM:WINDOWS"));
    po["libs"] += getAllLibs(po);
    
    po["rcflags"]  = addResDefine("WIN32") + po["____DEBUGRUNTIME_2_p_1"] + po["__NO_VC_CRTDBG_p_1"] + addResDefine("__WXMSW__");
    po["rcflags"] += po["__WXUNIV_DEFINE_p_1"] + po["__DEBUG_DEFINE_p_1"] + po["__EXCEPTIONS_DEFINE_p_1"];
    po["rcflags"] += po["__RTTI_DEFINE_p_1"] + po["__THREAD_DEFINE_p_1"] + po["__UNICODE_DEFINE_p_1"];
    po["rcflags"] += po["__MSLU_DEFINE_p_1"] + po["__GFXCTX_DEFINE_p_1"] + addResIncludeDir(po["SETUPHDIR"]);
    po["rcflags"] += addResIncludeDir(po["prefix"] + "\\include") + easyMode(addResIncludeDir("."));
    po["rcflags"] += po["__DLLFLAG_p_1"] + addResDefine("_WINDOWS");
    po["rcflags"] += easyMode(addResIncludeDir(po["prefix"] + "\\samples"));
    
    
    po["release"] = cfg["WXVER_MAJOR"] + "." + cfg["WXVER_MINOR"];
    po["version"] = cfg["WXVER_MAJOR"] + "." + cfg["WXVER_MINOR"] + "." + cfg["WXVER_RELEASE"];
    po["basename"]  = "wx" + po["PORTNAME"] + po["WXUNIVNAME"] + po["WX_RELEASE_NODOT"];
    po["basename"] += po["WXUNICODEFLAG"] + po["WXDEBUGFLAG"] + cfg["WX_LIB_FLAVOUR"];
    po["cc"] = m_programs.cc;
    po["cxx"] = m_programs.cxx;
    po["ld"] = m_programs.ld;
    
    getVariablesValues(po, cl, cfg);
  }
};

// -------------------------------------------------------------------------------------------------

/// OpenWatcom compiler
class CompilerWAT : public Compiler
{
public:
  CompilerWAT() : Compiler("wat")
  {
    m_programs.cc = "wcl386";//TODO: wcc386
    m_programs.cxx = "wcl386";//TODO: wpp386
    m_programs.ld = "wcl386";
    m_programs.lib = "wlib";
    m_programs.windres = "wrc";
    
    m_switches.includeDirs = "-i=";
    m_switches.resIncludeDirs = "-i=";
    m_switches.libDirs = "libp ";
    m_switches.linkLibs = "";
    m_switches.libPrefix = "libr ";
    m_switches.libExtension = "lib";
    m_switches.defines = "-d";
    m_switches.resDefines = "-d";
    m_switches.genericSwitch = "-";
    m_switches.forceCompilerUseQuotes = false;
    m_switches.forceLinkerUseQuotes = false;
    m_switches.linkerNeedsLibPrefix = true;//TODO:!
    m_switches.linkerNeedsLibExtension = true;
  }
  
  void process(Options& po, const CmdLineOptions& cl)
  {
    /// Searchs for '<prefix>\build\msw\config.*' first
    std::string cfg_first = po["prefix"] + "\\build\\msw\\config." + getName();
    
    /// config.* options
    BuildFileOptions cfg(cfg_first);
    
    /// build.cfg options
    cfg.parse(po["wxcfgfile"]);
    
    
    // ### Variables: ###
    po["WX_RELEASE_NODOT"] = cfg["WXVER_MAJOR"] + cfg["WXVER_MINOR"];
    if (po["WX_RELEASE_NODOT"].empty())
      po["WX_RELEASE_NODOT"] = "26";
    
    // ### Conditionally set variables: ###
    if (cfg["USE_GUI"] == "0")
      po["PORTNAME"] = "base";
    
    if (cfg["USE_GUI"] == "1")
      po["PORTNAME"] = "msw";
    
    if (cfg["BUILD"] == "debug" && cfg["DEBUG_FLAG"] == "default")
      po["WXDEBUGFLAG"] = "d";
    
    if (cfg["DEBUG_FLAG"] == "1")
      po["WXDEBUGFLAG"] = "d";
    
    if (cfg["UNICODE"] == "1")
      po["WXUNICODEFLAG"] = "u";
    
    if (cfg["WXUNIV"] == "1")
      po["WXUNIVNAME"] = "univ";
    
    if (cfg["SHARED"] == "1")
      po["WXDLLFLAG"] = "dll";
    
    if (cfg["SHARED"] == "0")
      po["LIBTYPE_SUFFIX"] = "lib";
    
    if (cfg["SHARED"] == "1")
      po["LIBTYPE_SUFFIX"] = "dll";
    
    if (cfg["MONOLITHIC"] == "0")
      po["EXTRALIBS_FOR_BASE"] = "";
    
    if (cfg["MONOLITHIC"] == "1")
      po["EXTRALIBS_FOR_BASE"] = "";
    //---------till here, the same
    if (cfg["BUILD"] == "debug" && cfg["DEBUG_INFO"] == "default")
      po["__DEBUGINFO_0"] = addFlag("-d2");
    
    if (cfg["BUILD"] == "release" && cfg["DEBUG_INFO"] == "default")
      po["__DEBUGINFO_0"] = addFlag("-d0");
    
    if (cfg["DEBUG_INFO"] == "0")
      po["__DEBUGINFO_0"] = addFlag("-d0");
    
    if (cfg["DEBUG_INFO"] == "1")
      po["__DEBUGINFO_0"] = addFlag("-d2");
    
    if (cfg["BUILD"] == "debug" && cfg["DEBUG_INFO"] == "default")
      po["__DEBUGINFO_1"] = addFlag("debug all"); //TODO: ???
    
    if (cfg["BUILD"] == "release" && cfg["DEBUG_INFO"] == "default")
      po["__DEBUGINFO_1"] = addFlag(""); //TODO: ???
    
    if (cfg["DEBUG_INFO"] == "0")
      po["__DEBUGINFO_1"] = addFlag(""); //TODO: ???
    
    if (cfg["DEBUG_INFO"] == "1")
      po["__DEBUGINFO_1"] = addFlag("debug all"); //TODO: ???
    //---------from here, the same
    if (cfg["BUILD"] == "debug")
      po["__OPTIMIZEFLAG_2"] = addFlag("-od");
    
    if (cfg["BUILD"] == "release")
      po["__OPTIMIZEFLAG_2"] = addFlag("-ot -ox");
    //------from here, almost the same
    if (cfg["USE_THREADS"] == "0")
      po["__THREADSFLAG_5"] = addFlag("");
    
    if (cfg["USE_THREADS"] == "1")
      po["__THREADSFLAG_5"] = addFlag("-bm");
    //---------from here, simmilar to VC
    if (cfg["RUNTIME_LIBS"] == "dynamic")
      po["__RUNTIME_LIBS_6"] = addFlag("-br");
    
    if (cfg["RUNTIME_LIBS"] == "static")
      po["__RUNTIME_LIBS_6"] = addFlag("");
    //---------simmilar:
    if (cfg["USE_RTTI"] == "0")
      po["__RTTIFLAG_7"] = addFlag("");
    
    if (cfg["USE_RTTI"] == "1")
      po["__RTTIFLAG_7"] = addFlag("-xr");
    
    if (cfg["USE_EXCEPTIONS"] == "0")
      po["__EXCEPTIONSFLAG_8"] = addFlag("");
    
    if (cfg["USE_EXCEPTIONS"] == "1")
      po["__EXCEPTIONSFLAG_8"] = addFlag("-xs");
    
    process_3(po, cl, cfg);
    
    //---------the same, but without resDefines:
    if (cfg["WXUNIV"] == "1")
      po["__WXUNIV_DEFINE_p"] = addDefine("__WXUNIVERSAL__");
    
    if (cfg["BUILD"] == "debug" && cfg["DEBUG_FLAG"] == "default")
      po["__DEBUG_DEFINE_p"] = addDefine("__WXDEBUG__");
    
    if (cfg["DEBUG_FLAG"] == "1")
      po["__DEBUG_DEFINE_p"] = addDefine("__WXDEBUG__");
    
    if (cfg["USE_EXCEPTIONS"] == "0")
      po["__EXCEPTIONS_DEFINE_p"] = addDefine("wxNO_EXCEPTIONS");
    
    if (cfg["USE_RTTI"] == "0")
      po["__RTTI_DEFINE_p"] = addDefine("wxNO_RTTI");
    
    if (cfg["USE_THREADS"] == "0")
      po["__THREAD_DEFINE_p"] = addDefine("wxNO_THREADS");
    
    if (cfg["UNICODE"] == "1")
      po["__UNICODE_DEFINE_p"] = addDefine("_UNICODE");
    
    if (cfg["MSLU"] == "1")
      po["__MSLU_DEFINE_p"] = addDefine("wxUSE_UNICODE_MSLU=1");
    
    if (cfg["USE_GDIPLUS"] == "1")
      po["__GFXCTX_DEFINE_p"] = addDefine("wxUSE_GRAPHICS_CONTEXT=1");
    
    if (cfg["SHARED"] == "1")
      po["__DLLFLAG_p"] = addDefine("WXUSINGDLL");
    
    
    // ### Variables, Part 2: ###
    po["LIBDIRNAME"] = po["prefix"] + "\\lib\\" + getName() + "_" + po["LIBTYPE_SUFFIX"] + cfg["CFG"];
    
    po["SETUPHDIR"]  = po["LIBDIRNAME"] + "\\" + po["PORTNAME"] + po["WXUNIVNAME"];
    po["SETUPHDIR"] += po["WXUNICODEFLAG"] + po["WXDEBUGFLAG"];
    
    po["cflags"]  = easyMode(po["__DEBUGINFO_0"]) + easyMode(po["__OPTIMIZEFLAG_2"]) + po["__THREADSFLAG_5"];
    po["cflags"] += po["__RUNTIME_LIBS_6"] + addDefine("__WXMSW__") + po["__WXUNIV_DEFINE_p"];
    po["cflags"] += po["__DEBUG_DEFINE_p"] + po["__EXCEPTIONS_DEFINE_p"] + po["__RTTI_DEFINE_p"];
    po["cflags"] += po["__THREAD_DEFINE_p"] + po["__UNICODE_DEFINE_p"] + po["__MSLU_DEFINE_p"] + po["__GFXCTX_DEFINE_p"];
    po["cflags"] += addIncludeDir(po["SETUPHDIR"]) + addIncludeDir(po["prefix"] + "\\include");
    po["cflags"] += addFlag("-wx") + addFlag("-wcd=549") + addFlag("-wcd=656") + addFlag("-wcd=657") + addFlag("-wcd=667");
    po["cflags"] += easyMode(addIncludeDir(".")) + po["__DLLFLAG_p"];
    po["cflags"] += easyMode(addIncludeDir(po["prefix"] + "\\samples")) + addDefine("NOPCH") + po["__RTTIFLAG_7"] + po["__EXCEPTIONSFLAG_8"];
    po["cflags"] += cfg["CPPFLAGS"] + " " + cfg["CXXFLAGS"] + " ";
    
    po["lbc"]  = "option quiet\n";
    po["lbc"] += "name $^@\n";
    po["lbc"] += "option caseexact\n";
    po["libs"]  = cfg["LDFLAGS"] + " ";
    po["libs"] += easyMode(po["__DEBUGINFO_1"]);
    po["libs"] += addLinkerDir(po["LIBDIRNAME"]);
    po["lbc"] += "    libpath " + po["LIBDIRNAME"] + " system nt_win ref '_WinMain@16'";
    po["libs"] += getAllLibs(po);
    
    po["rcflags"]  = addFlag("-q") + addFlag("-ad") + addFlag("-bt=nt") + addFlag("-r");
    po["rcflags"] += addResDefine("__WXMSW__") + po["__WXUNIV_DEFINE_p"];
    po["rcflags"] += po["__DEBUG_DEFINE_p"] + po["__EXCEPTIONS_DEFINE_p"];
    po["rcflags"] += po["__RTTI_DEFINE_p"] + po["__THREAD_DEFINE_p"] + po["__UNICODE_DEFINE_p"];
    po["rcflags"] += po["__MSLU_DEFINE_p"] + po["__GFXCTX_DEFINE_p"] + addResIncludeDir(po["SETUPHDIR"]);
    po["rcflags"] += addResIncludeDir(po["prefix"] + "\\include") + easyMode(addResIncludeDir("."));
    po["rcflags"] += po["__DLLFLAG_p_1"];
    po["rcflags"] += easyMode(addResIncludeDir(po["prefix"] + "\\samples"));
    
    po["release"] = cfg["WXVER_MAJOR"] + "." + cfg["WXVER_MINOR"];
    po["version"] = cfg["WXVER_MAJOR"] + "." + cfg["WXVER_MINOR"] + "." + cfg["WXVER_RELEASE"];
    po["basename"]  = "wx" + po["PORTNAME"] + po["WXUNIVNAME"] + po["WX_RELEASE_NODOT"];
    po["basename"] += po["WXUNICODEFLAG"] + po["WXDEBUGFLAG"] + cfg["WX_LIB_FLAVOUR"];
    po["cc"] = m_programs.cc;
    po["cxx"] = m_programs.cxx;
    po["ld"] = m_programs.ld;
    
    getVariablesValues(po, cl, cfg);
  }
};

// -------------------------------------------------------------------------------------------------

/// Borland compiler
class CompilerBCC : public Compiler
{
public:
  CompilerBCC() : Compiler("bcc")
  {
    m_programs.cc = "bcc32";
    m_programs.cxx = "bcc32";
    m_programs.ld = "ilink32";
    m_programs.lib = "tlib";
    m_programs.windres = "brcc32";
    
    m_switches.includeDirs = "-I";
    m_switches.resIncludeDirs = "--include-dir"; ////////////
    m_switches.libDirs = "-L";
    m_switches.linkLibs = "";
    m_switches.libPrefix = "";
    m_switches.libExtension = "lib";
    m_switches.defines = "-D";
    m_switches.resDefines = "/d";  ////////
    m_switches.genericSwitch = "-";
    m_switches.forceCompilerUseQuotes = false;
    m_switches.forceLinkerUseQuotes = true;
    m_switches.linkerNeedsLibPrefix = false;
    m_switches.linkerNeedsLibExtension = true;
  }
  
  void process(Options& po, const CmdLineOptions& cl) {}
};

// -------------------------------------------------------------------------------------------------

// Get the base directory for wxWidgets installation
std::string* getPrefix(CmdLineOptions& cl)
{
  if (cl.keyExists("--prefix"))
    return new std::string(cl["--prefix"]);
  else if (getenv("WXWIN"))
    return new std::string(getenv("WXWIN"));
  else
  {
    /// Assume that, like a *nix, we're installed in C:\some\path\bin,
    /// and that the root dir of wxWidgets is therefore C:\some\path.
    DWORD length = MAX_PATH;
    LPSTR libPath = NULL;
    LPSTR filePart;
    
    /// Why, oh why, doesn't MSFT give us a way to find the actual return length,
    /// like they do with everything else?!?
    do
    {
      libPath = new CHAR[length];
      GetModuleFileName(NULL, libPath, length);
      GetFullPathName(libPath, length, libPath, &filePart);
      *filePart = '\0';
      // Add 3 for ".." and NULL
      if (strlen(libPath) + 3 > length)
      {
        length *= 2;
        delete[] libPath;
        libPath = NULL;
      }
    } while (libPath == NULL);
    strcpy(filePart, "..");
        
    // Fix the .. part
    GetFullPathName(libPath, length, libPath, NULL);
    return new std::string(libPath);
  }
}

// -------------------------------------------------------------------------------------------------

void normalizePath(std::string& path)
{
  // converts all slashes to backslashes
  std::replace(path.begin(), path.end(), '/', '\\');
  
  // removes the first slash (if any) from the given path
  std::string::iterator firstChar = path.begin();
  if (*firstChar == '\\')
    path.erase(firstChar);
  
  // removes the last slash (if any) from the given path
  std::string::iterator lastChar = path.end() - 1;
  if (*lastChar == '\\')
    path.erase(lastChar);
}

// -------------------------------------------------------------------------------------------------

/// Return true if string changed
bool replaceCompilerIfFound(std::string& wxcfg, const std::string& compiler)
{
  size_t loc;
  loc = wxcfg.find(compiler);
  if (loc != std::string::npos)
    return false;
  
  std::vector<std::string> compilers;
  compilers.push_back("gcc_");
  compilers.push_back("dmc_");
  compilers.push_back("vc_");
  compilers.push_back("wat_");
  compilers.push_back("bcc_");
  
  for(std::vector<std::string>::const_iterator it = compilers.begin(); it != compilers.end(); ++it)
  {
    loc = wxcfg.find(*it);
    if (loc != std::string::npos)
    {
      wxcfg.replace(loc, (*it).length(), compiler + "_");
      return true;
    }
  }
  return false;
}

// -------------------------------------------------------------------------------------------------

/// Return true if string changed
bool replaceUniv(std::string& wxcfg, bool enable)
{
  const std::string univStr("univ");
  size_t univPos = wxcfg.rfind(univStr);
  
  if (enable)
  {
    /// Pattern: Replace /(msw|base)/ to /(msw|base)univ/
    if (univPos == std::string::npos)
    {
      const std::string mswStr("msw");
      size_t mswPos = wxcfg.rfind(mswStr);
      if (mswPos != std::string::npos)
        wxcfg.insert(mswPos + mswStr.length(), univStr);
      else
      {
        const std::string baseStr("base");
        size_t basePos = wxcfg.rfind(baseStr);
        if (basePos != std::string::npos)
          wxcfg.insert(basePos + baseStr.length(), univStr);
      }
      return true;
    }
  }   
  else
  {
    /// Pattern: Remove /univ/ in /(msw|base)univ/
    if (univPos != std::string::npos)
    {
      wxcfg.erase(univPos, univStr.length());
      return true;
    }
  }
  return false;
}

// -------------------------------------------------------------------------------------------------

/// Return true if string changed
bool replaceUnicode(std::string& wxcfg, bool enable)
{
  std::string::iterator lastChar = wxcfg.end() - 1;
  
  if (enable)
  {
    /// Pattern: Add /.*u/ if it's not already or /.*ud/ if --debug is specified
    // TODO: string::find could be better
    if (*lastChar != 'u' && *lastChar != 'd')
    {
      wxcfg += "u";
      return true;
    }
    else if (*(lastChar - 1) != 'u' && *lastChar == 'd')
    {
      *lastChar = 'u';
      wxcfg += "d";
      return true;
    }
  }
  else
  {
    /// Pattern: Remove /.*u/ if it's present or /.*ud/ if --debug is specified
    if (*lastChar == 'u')
    {
      wxcfg.erase(lastChar);
      return true;
    }
    else if (*(lastChar - 1) == 'u' && *lastChar == 'd')
    {
      wxcfg.erase(lastChar - 1);
      return true;
    }
  }
  return false;
}

// -------------------------------------------------------------------------------------------------

/// Return true if string changed
bool replaceDebug(std::string& wxcfg, bool enable)
{
  std::string::iterator lastChar = wxcfg.end() - 1;
    
  if (enable)
  {
    /// Pattern: Add /.*d/ if it's not already
    if (*lastChar != 'd')
    {
      wxcfg += "d";
      return true;
    }
  }
  else
  {
    /// Pattern: Remove /.*d/ if it's present
    if (*lastChar == 'd')
    {
      wxcfg.erase(lastChar);
      return true;
    }
  }
  return false;
}

// -------------------------------------------------------------------------------------------------

/// Return true if string changed
bool replaceStatic(std::string& wxcfg, bool enable)
{
  if (enable)
  {
    /// Pattern: Replace /.*_dll/ to /.*_lib/
    size_t loc = wxcfg.find("_dll");
    if (loc != std::string::npos)
    {
      wxcfg.replace(loc, std::string("_lib").length(), "_lib");
      return true;
    }
  }
  else
  {
    /// Pattern: Replace /.*_lib/ to /.*_dll/
    size_t loc = wxcfg.find("_lib");
    if (loc != std::string::npos)
    {
      wxcfg.replace(loc, std::string("_dll").length(), "_dll");
      return true;
    }
  }
  return false;
}

// -------------------------------------------------------------------------------------------------

void autodetectConfiguration(Options& po, const CmdLineOptions& cl)
{
  // TODO: still directory listing is needed, to account for $(CFG), $(DIR_SUFFIX_CPU), ...
  
  std::vector<std::string> cfgs;
  std::vector<std::string> newCfgs;
  std::string              cfg;
  std::vector<std::string>::iterator curCfg;
  
  // Iterate through the options the user didn't supply
  cfgs.push_back(po["wxcfg"]);
  if (!cl.keyExists("--universal"))
  {
    cfg = po["wxcfg"];
    if (replaceUniv(cfg, true))
      cfgs.push_back(cfg);
    cfg = po["wxcfg"];
    if (replaceUniv(cfg, false))
      cfgs.push_back(cfg);
  }
  if (!cl.keyExists("--unicode"))
  {
    newCfgs.clear();
    for (curCfg = cfgs.begin(); curCfg != cfgs.end(); ++curCfg)
    {
      cfg = *curCfg;
      if (replaceUnicode(cfg, true))
        newCfgs.push_back(cfg);
      cfg = *curCfg;
      if (replaceUnicode(cfg, false))
        newCfgs.push_back(cfg);
    }
    for (curCfg = newCfgs.begin(); curCfg != newCfgs.end(); ++curCfg)
      cfgs.push_back(*curCfg);
  }
  if (!cl.keyExists("--debug"))
  {
    newCfgs.clear();
    for (curCfg = cfgs.begin(); curCfg != cfgs.end(); ++curCfg)
    {
      cfg = *curCfg;
      if (replaceDebug(cfg, true))
        newCfgs.push_back(cfg);
      cfg = *curCfg;
      if (replaceDebug(cfg, false))
        newCfgs.push_back(cfg);
    }
    for (curCfg = newCfgs.begin(); curCfg != newCfgs.end(); ++curCfg)
      cfgs.push_back(*curCfg);
  }
  if (!cl.keyExists("--static"))
  {
    newCfgs.clear();
    for (curCfg = cfgs.begin(); curCfg != cfgs.end(); ++curCfg)
    {
      cfg = *curCfg;
      if (replaceStatic(cfg, true))
        newCfgs.push_back(cfg);
      cfg = *curCfg;
      if (replaceStatic(cfg, false))
        newCfgs.push_back(cfg);
    }
    for (curCfg = newCfgs.begin(); curCfg != newCfgs.end(); ++curCfg)
      cfgs.push_back(*curCfg);
  }
  if (!cl.keyExists("--compiler"))
  {
    newCfgs.clear();
    for (curCfg = cfgs.begin(); curCfg != cfgs.end(); ++curCfg)
    {
      cfg = *curCfg;
      if (replaceCompilerIfFound(cfg, "gcc"))
        newCfgs.push_back(cfg);
      cfg = *curCfg;
      if (replaceCompilerIfFound(cfg, "dmc"))
        newCfgs.push_back(cfg);
      cfg = *curCfg;
      if (replaceCompilerIfFound(cfg, "vc"))
        newCfgs.push_back(cfg);
      cfg = *curCfg;
      if (replaceCompilerIfFound(cfg, "wat"))
        newCfgs.push_back(cfg);
      cfg = *curCfg;
      if (replaceCompilerIfFound(cfg, "bcc"))
        newCfgs.push_back(cfg);
    }
    for (curCfg = newCfgs.begin(); curCfg != newCfgs.end(); ++curCfg)
      cfgs.push_back(*curCfg);
  }
  
  
    // reads the first setup.h it founds
  bool found = false;
  for(std::vector<std::string>::const_iterator it = cfgs.begin(); it != cfgs.end(); ++it)
  {
    std::string file = po["prefix"] + "\\lib\\" + *it + "\\wx\\setup.h";
    std::ifstream setupH(file.c_str());
    if (setupH.is_open())
    {
      if (!found)
      {
        po["wxcfg"] = *it;
        found = true;
      }
      else
      {
        std::cerr << g_tokWarning << "Multiple compiled configurations of wxWidgets have been detected." << std::endl;
        std::cerr << "Using first detected version by default." << std::endl;
        std::cerr << std::endl;
        std::cerr << "Please use the --wxcfg flag (as in wx-config --wxcfg=gcc_dll\\mswud)" << std::endl;
        std::cerr << "or set the environment variable WXCFG (as in WXCFG=gcc_dll\\mswud)" << std::endl;
        std::cerr << "to specify which configuration exactly you want to use." << std::endl;
        return;
      }
    }
  }
  
  if (!po.keyExists("wxcfg"))
  {
    // TODO: this never reaches thanks to the new autodetection algorithm
    
    std::cout << g_tokError << "No setup.h file has been auto-detected." << std::endl;
    std::cerr << std::endl;
    std::cerr << "Please use the --wxcfg flag (as in wx-config --wxcfg=gcc_dll\\mswud)" << std::endl;
    std::cerr << "or set the environment variable WXCFG (as in WXCFG=gcc_dll\\mswud)" << std::endl;
    std::cerr << "to specify which configuration exactly you want to use." << std::endl;
    
    exit(1);
  }
}

// -------------------------------------------------------------------------------------------------

void checkAdditionalFlags(Options& po, const CmdLineOptions& cl)
{
  /// Modifies wxcfg as 'vc[cpu]_[dll|lib][CFG]\[msw|base][univ][u][d]' accordingly
  /// or 'gcc_[dll|lib][CFG]\[msw|base][univ][u][d]'

  if (cl.keyExists("--universal"))
  {
    if (cl.keyValue("--universal") == "no")
      replaceUniv(po["wxcfg"], false);
    else if (cl.keyValue("--universal") == "yes" || cl.keyValue("--universal").empty())
      replaceUniv(po["wxcfg"], true);
  }

  if (cl.keyExists("--unicode"))
  {
    if (cl.keyValue("--unicode") == "no")
      replaceUnicode(po["wxcfg"], false);
    else if (cl.keyValue("--unicode") == "yes" || cl.keyValue("--unicode").empty())
      replaceUnicode(po["wxcfg"], true);
  }
  
  if (cl.keyExists("--debug"))
  {
    if (cl.keyValue("--debug") == "no")
      replaceDebug(po["wxcfg"], false);
    else if (cl.keyValue("--debug") == "yes" || cl.keyValue("--debug").empty())
      replaceDebug(po["wxcfg"], true);
  }
  
  if (cl.keyExists("--static"))
  {
    if (cl.keyValue("--static") == "no")
      replaceStatic(po["wxcfg"], false);
    else if (cl.keyValue("--static") == "yes" || cl.keyValue("--static").empty())
      replaceStatic(po["wxcfg"], true);
  }
  
  if (cl.keyExists("--compiler"))
  {
    if (cl.keyValue("--compiler") == "gcc")
      replaceCompilerIfFound(po["wxcfg"], "gcc");
    else if (cl.keyValue("--compiler") == "dmc")
      replaceCompilerIfFound(po["wxcfg"], "dmc");
    else if (cl.keyValue("--compiler") == "vc")
      replaceCompilerIfFound(po["wxcfg"], "vc");
    else if (cl.keyValue("--compiler") == "wat")
      replaceCompilerIfFound(po["wxcfg"], "wat");
    else if (cl.keyValue("--compiler") == "bcc")
      replaceCompilerIfFound(po["wxcfg"], "bcc");
  }
}

// -------------------------------------------------------------------------------------------------


void detectCompiler(Options& po, const CmdLineOptions& cl)
{
  // input example of po["wxcfg"]:
  // gcc_dll\\mswud
  // vc_lib\\msw
  
  if (po["wxcfg"].find("gcc_") != std::string::npos)
  {
    CompilerMinGW compiler;
    compiler.process(po, cl);
    return;
  }
  else if (po["wxcfg"].find("dmc_") != std::string::npos)
  {
    CompilerDMC compiler;
    compiler.process(po, cl);
    return;
  }
  else if (po["wxcfg"].find("vc_") != std::string::npos)
  {
    CompilerVC compiler;
    compiler.process(po, cl);
    return;
  }
  else if (po["wxcfg"].find("wat_") != std::string::npos)
  {
    CompilerWAT compiler;
    compiler.process(po, cl);
    return;
  }
  else if (po["wxcfg"].find("bcc_") != std::string::npos)
  {
    CompilerBCC compiler;
    compiler.process(po, cl);
    return;
  }
  else
  {
    // TODO: this never reaches thanks to the new autodetection algorithm
    std::cout << g_tokError << "No supported compiler has been detected in the configuration '"
              << po["wxcfg"] << "'." << std::endl;
    std::cerr << std::endl;
    std::cerr << "The specified wxcfg must start with a 'gcc_', 'dmc_' or 'vc_'" << std::endl;
    std::cerr << "to be successfully detected." << std::endl;

    exit(1);
  }
}

// -------------------------------------------------------------------------------------------------

void validatePrefix(const std::string& prefix)
{
  // tests if prefix is a valid dir. checking if there is an \include\wx\wx.h
  const std::string testfile = prefix + "\\include\\wx\\wx.h";
  std::ifstream prefixIsValid(testfile.c_str());
  if (!prefixIsValid.is_open())
  {
    std::cout << g_tokError << "wxWidgets hasn't been found installed at '" << prefix << "'." << std::endl;
    std::cerr << std::endl;
    std::cerr << "Please use the --prefix flag (as in wx-config --prefix=C:\\wxWidgets)" << std::endl;
    std::cerr << "or set the environment variable WXWIN (as in WXWIN=C:\\wxWidgets)" << std::endl;
    std::cerr << "to specify where is your installation of wxWidgets." << std::endl;
    
    exit(1);
  }
}

// -------------------------------------------------------------------------------------------------

bool validateConfiguration(const std::string& wxcfgfile, const std::string& wxcfgsetuphfile, bool exitIfError = true)
{
  std::ifstream build_cfg(wxcfgfile.c_str());
  std::ifstream setup_h(wxcfgsetuphfile.c_str());

  bool isBuildCfgOpen = build_cfg.is_open();
  bool isSetupHOpen = setup_h.is_open();

  if (!isBuildCfgOpen && exitIfError)
  {
    if (!isSetupHOpen && exitIfError)
    {
      std::cout << g_tokError << "No valid setup.h of wxWidgets has been found at location: "
                << wxcfgsetuphfile << std::endl;
      std::cerr << std::endl;
      std::cerr << "Please use the --wxcfg flag (as in wx-config --wxcfg=gcc_dll\\mswud)" << std::endl;
      std::cerr << "or set the environment variable WXCFG (as in WXCFG=gcc_dll\\mswud)" << std::endl;
      std::cerr << "to specify which configuration exactly you want to use." << std::endl;
      
      exit(1);
    }
    
    std::cout << g_tokError << "No valid configuration of wxWidgets has been found at location: "
              << wxcfgfile << std::endl;
    std::cerr << std::endl;
    std::cerr << "Please use the --wxcfg flag (as in wx-config --wxcfg=gcc_dll\\mswud)" << std::endl;
    std::cerr << "or set the environment variable WXCFG (as in WXCFG=gcc_dll\\mswud)" << std::endl;
    std::cerr << "to specify which configuration exactly you want to use." << std::endl;
    
    exit(1);
  }
  return isBuildCfgOpen && isSetupHOpen;
}

// -------------------------------------------------------------------------------------------------

void outputFlags(Options& po, const CmdLineOptions& cl)
{
  /// Outputs flags to console
  if (cl.keyExists("--variable"))
  {
    std::cout << po["variable"];
    exit(0);
  }
  if (cl.keyExists("--cc"))
    std::cout << po["cc"];
  if (cl.keyExists("--cxx"))
    std::cout << po["cxx"];
  if (cl.keyExists("--ld"))
    std::cout << po["ld"];
  if (cl.keyExists("--cflags") || cl.keyExists("--cxxflags") || cl.keyExists("--cppflags"))
    std::cout << po["cflags"] << std::endl;
  if (cl.keyExists("--libs"))
    std::cout << po["libs"] << std::endl;
  if (cl.keyExists("--rcflags"))
    std::cout << po["rcflags"] << std::endl;
  if (cl.keyExists("--release"))
    std::cout << po["release"];
  if (cl.keyExists("--version"))
    std::cout << po["version"];
  if (cl.keyExists("--basename"))
    std::cout << po["basename"];
}

// -------------------------------------------------------------------------------------------------

// Get the base directory for wxWidgets installation
void getWxCfg(CmdLineOptions& cl, Options& po)
{
  if (cl.keyExists("--wxcfg"))
    po["wxcfg"] = cl["--wxcfg"];
  else if (getenv("WXCFG") && !cl.keyExists("--prefix"))
    po["wxcfg"] = getenv("WXCFG");
  else
  {
    // Try if something valid can be found trough deriving checkAdditionalFlags() first
    po["wxcfg"] = "gcc_dll\\msw";
    po["wxcfgfile"] = po["prefix"] + "\\lib\\" + po["wxcfg"] + "\\build.cfg";
    po["wxcfgsetuphfile"] = po["prefix"] + "\\lib\\" + po["wxcfg"] + "\\wx\\setup.h";
    checkAdditionalFlags(po, cl);
    
    if (!validateConfiguration(po["wxcfgfile"], po["wxcfgsetuphfile"], false))
      autodetectConfiguration(po, cl);    // important function
  }
}

// -------------------------------------------------------------------------------------------------



int main(int argc, char* argv[])
{
  /// Command line options
  CmdLineOptions cl(argc, argv);

  /// Program options
  Options po;

  if (!cl.validArgs())
    return 1;

  if (cl.keyExists("-v"))
  {
    std::cout << "wx-config revision " << getSvnRevision() << " " << getSvnDate() << std::endl;
    return 0;
  }
  
  checkEasyMode(cl);

  // Get and normalize the base wxWidgets install location
  std::string* pPrefix = getPrefix(cl);
  po["prefix"] = *pPrefix;
  normalizePath(po["prefix"]);
  validatePrefix(po["prefix"]);
  
  getWxCfg(cl, po);
  normalizePath(po["wxcfg"]);
  checkAdditionalFlags(po, cl);
  po["wxcfgfile"] = po["prefix"] + "\\lib\\" + po["wxcfg"] + "\\build.cfg";
  po["wxcfgsetuphfile"] = po["prefix"] + "\\lib\\" + po["wxcfg"] + "\\wx\\setup.h";
  validateConfiguration(po["wxcfgfile"], po["wxcfgsetuphfile"]);
  
  detectCompiler(po, cl);
  outputFlags(po, cl);    
  
  return 0;
}
