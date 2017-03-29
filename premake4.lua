#!lua

-- A solution contains projects, and defines the available configurations
solution "SuDiTE"
	location "build"
	language "C++"

	configurations { "release", "debug" }

	function DefaultConfig(complimentName)
		configuration "Debug"
			defines { "DEBUG", "_DEBUG" }
			flags { "Symbols" }
			targetdir ("build/debug/" .. complimentName)

		configuration "Release"
			defines { "NDEBUG", "BOOST_DISABLE_ASSERTS" }
			flags { "Optimize" }
			targetdir ("build/release/" .. complimentName)


		configuration "*"
	end

	project "SharedTools"
		DefaultConfig("lib")
		kind "StaticLib"
		includedirs { 
			"Tools/inc/",
			"boost/", -- There is no search for the include dirs (in particular on windows it is prety difficult
		}
		files{ "Tools/inc/**.h", "Tools/src/**.cpp" }

		configuration "Debug"
			targetname( "SharedTools" )

		configuration "Release"
			targetname( "SharedTools" )

	project "SuDiTECommon"
		DefaultConfig("lib")
		kind "StaticLib"
		includedirs { 
			"Tools/inc/",
			"SuDiTECommon/inc/",
			"boost/", -- There is no search for the include dirs (in particular on windows it is prety difficult
		}
		files{ "SuDiTECommon/inc/**.h", "SuDiTECommon/src/**.cpp" }

		configuration "Debug"
			targetname( "SuDiTECommon" )

		configuration "Release"
			targetname( "SuDiTECommon" )

