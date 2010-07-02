/***
 * millipede: WindowSettings.h
 * Copyright Stuart Golodetz, 2009. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_WINDOWSETTINGS
#define H_MILLIPEDE_WINDOWSETTINGS

namespace mp {

class WindowSettings
{
	//#################### PRIVATE VARIABLES ####################
private:
	double m_centre;
	double m_width;

	//#################### CONSTRUCTORS ####################
public:
	WindowSettings();
	WindowSettings(double centre, double width);

	//#################### PUBLIC METHODS ####################
public:
	double centre() const;
	bool unspecified() const;
	double width() const;
};

}

#endif
