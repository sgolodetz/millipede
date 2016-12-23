/***
 * millipede: FieldIO.tpp
 * Copyright Stuart Golodetz, 2009. All rights reserved.
 ***/

#include <boost/lexical_cast.hpp>

#include <millipede/exceptions/Exception.h>

namespace mp {

//#################### READING METHODS ####################
template <typename T>
T FieldIO::read_typed_field(std::istream& is, const std::string& expectedFieldName)
{
	std::string fieldString = read_field(is, expectedFieldName);
	try
	{
		T field = boost::lexical_cast<T,std::string>(fieldString);
		return field;
	}
	catch(boost::bad_lexical_cast&)
	{
		throw Exception("The value for " + expectedFieldName + " was not of the right type");
	}
}

template <typename T>
T FieldIO::read_typed_trimmed_field(std::istream& is, const std::string& expectedFieldName)
{
	std::string fieldString = read_trimmed_field(is, expectedFieldName);
	try
	{
		T field = boost::lexical_cast<T,std::string>(fieldString);
		return field;
	}
	catch(boost::bad_lexical_cast&)
	{
		throw Exception("The value for " + expectedFieldName + " was not of the right type");
	}
}

//#################### WRITING METHODS ####################
template <typename T>
void FieldIO::write_typed_field(std::ostream& os, const std::string& fieldName, const T& value)
{
	os << fieldName << " = " << value << '\n';
}

}
