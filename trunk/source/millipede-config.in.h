#ifndef H_MILLIPEDE_CONFIG
#define H_MILLIPEDE_CONFIG

#include <string>

namespace millipede {

struct Config
{
	static std::string tests_path()
	{
		return "${millipede_SOURCE_DIR}/tests";
	}
};

}

#endif
