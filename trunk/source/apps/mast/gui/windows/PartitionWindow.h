/***
 * millipede: PartitionWindow.h
 * Copyright Stuart Golodetz, 2009. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_PARTITIONWINDOW
#define H_MILLIPEDE_PARTITIONWINDOW

#include <boost/shared_ptr.hpp>
using boost::shared_ptr;

#include <wx/frame.h>

namespace mp {

//#################### FORWARD DECLARATIONS ####################
typedef shared_ptr<class Volume> Volume_Ptr;

class PartitionWindow : public wxFrame
{
	//#################### CONSTRUCTORS ####################
public:
	PartitionWindow(const std::string& title, const Volume_Ptr& volume);
};

}

#endif
