/***
 * millipede: MeshBuildingData.h
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_MESHBUILDINGDATA
#define H_MILLIPEDE_MESHBUILDINGDATA

#include <itkImage.h>

#include <common/jobs/DataHook.h>
#include "CubeFaceTable.h"

namespace mp {

template <typename Label>
class MeshBuildingData
{
	//#################### TYPEDEFS ####################
public:
	typedef itk::Image<Label,3> LabelImage;
	typedef typename LabelImage::Pointer LabelImagePointer;

	//#################### PRIVATE VARIABLES ####################
private:
	DataHook<LabelImagePointer> m_labellingHook;
	CubeFaceTable_Ptr m_cubeFaceTable;
	// TODO: Node map
	// TODO: Triangles

	//#################### CONSTRUCTORS ####################
public:
	MeshBuildingData()
	:	m_cubeFaceTable(new CubeFaceTable)
	{}

	//#################### PUBLIC METHODS ####################
public:
	void set_labelling(const LabelImagePointer& labelling)
	{
		m_labellingHook.set(labelling);
	}

	void set_labelling_hook(const DataHook<LabelImagePointer>& labellingHook)
	{
		m_labellingHook = labellingHook;
	}
};

}

#endif
