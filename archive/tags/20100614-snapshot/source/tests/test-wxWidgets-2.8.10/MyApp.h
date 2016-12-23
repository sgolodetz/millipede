#ifndef H_MYAPP
#define H_MYAPP

#include <wx/wx.h>

class MyApp : public wxApp
{
public:
	virtual bool OnInit();
};

DECLARE_APP(MyApp)
IMPLEMENT_APP(MyApp)

#endif
