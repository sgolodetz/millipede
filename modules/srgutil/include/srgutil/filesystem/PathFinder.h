/**
 * srgutil: PathFinder.h
 * Copyright Stuart Golodetz, 2016. All rights reserved.
 */

#ifndef H_SRGUTIL_PATHFINDER
#define H_SRGUTIL_PATHFINDER

#include <boost/filesystem.hpp>

namespace srgutil {

/**
 * \brief Finds the path to the current executable.
 *
 * \return  The path to the current executable.
 */
extern boost::filesystem::path find_executable();

/**
 * \brief Finds the path to a named subdirectory of the directory containing the current executable.
 *
 * \param name  The name of the subdirectory whose path we want to get.
 * \return      The path to the subdirectory with the specified name.
 */
extern boost::filesystem::path find_subdir_from_executable(const std::string& name);

}

#endif
