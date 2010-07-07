/***
 * millipede: CubeFaceGenerator.h
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_CUBEFACEGENERATOR
#define H_MILLIPEDE_CUBEFACEGENERATOR

#include <set>

#include <common/adts/Edge.h>
#include <common/exceptions/Exception.h>
#include <common/jobs/SimpleJob.h>
#include "CubeFaceDesignator.h"
#include "MeshBuildingData.h"

namespace mp {

/**
@brief	A CubeFaceGenerator determines the multiple material marching squares (M3S) pattern on a cube face
		and generates global nodes and edges (stored implicitly in the nodes) accordingly. It also builds
		a map from local to global nodes and stores this in the cube face table.

@tparam	Label			The type of label to be used
@tparam	PriorityPred	A predicate type defining an ordering over the labels for resolving conflicts that arise during the algorithm
*/
template <typename Label, typename PriorityPred>
class CubeFaceGenerator : public SimpleJob
{
	//#################### TYPEDEFS ####################
private:
	typedef itk::Image<Label,3> LabelImage;
	typedef typename LabelImage::Pointer LabelImagePointer;
	typedef MeshBuildingData<Label> MeshBuildingDataT;
	typedef boost::shared_ptr<MeshBuildingDataT> MeshBuildingData_Ptr;

	//#################### PRIVATE VARIABLES ####################
private:
	MeshBuildingData_Ptr m_data;
	CubeFaceDesignator::Enum m_faceDesignator;
	int m_x, m_y, m_z;
	LabelImagePointer m_labelling;

	//#################### CONSTRUCTORS ####################
public:
	CubeFaceGenerator(const MeshBuildingData_Ptr& data, int x, int y, int z, CubeFaceDesignator::Enum faceDesignator)
	:	m_data(data), m_faceDesignator(faceDesignator), m_x(x), m_y(y), m_z(z)
	{}

	//#################### PUBLIC METHODS ####################
public:
	int length() const
	{
		return 1;
	}

	//#################### PRIVATE METHODS ####################
private:
	static std::list<Edge> edges_on_face(Label topleft, Label topright, Label bottomleft, Label bottomright)
	{
		std::list<Edge> edges;

		std::set<Label, PriorityPred> labels;
		labels.insert(topleft);
		labels.insert(topright);
		labels.insert(bottomleft);
		labels.insert(bottomright);

		const int labelCount = static_cast<int>(labels.size());

		switch(labelCount)
		{
			case 1:
			case 2:
			{
				int a = topright != topleft, b = bottomleft != topleft, c = bottomright != topleft;
				int combo = (a << 2) + (b << 1) + c;
				switch(combo)
				{
					case 0:		// 0000 or 1111
						break;
					case 1:		// 0001 or 1110
						edges.push_back(Edge(3, 4));
						break;
					case 2:		// 0010 or 1101
						edges.push_back(Edge(1, 4));
						break;
					case 3:		// 0011 or 1100
						edges.push_back(Edge(1, 3));
						break;
					case 4:		// 0100 or 1011
						edges.push_back(Edge(3, 0));
						break;
					case 5:		// 0101 or 1010
						edges.push_back(Edge(0, 4));
						break;
					case 6:		// 0110 or 1001
						// Ambiguous case: use label priorities to resolve.
						if(PriorityPred()(topleft, topright))	// if the topleft label has priority over the topright one
						{
							edges.push_back(Edge(0, 3));
							edges.push_back(Edge(1, 4));
						}
						else
						{
							edges.push_back(Edge(0, 1));
							edges.push_back(Edge(3, 4));
						}
						break;
					case 7:		// 0111 or 1000
						edges.push_back(Edge(1, 0));
						break;
				}
				break;
			}
			case 3:
			{
				// Cases: tl=tr, tl=bl, tl=br, tr=bl, tr=br, bl=br
				if(topleft == topright)
				{
					edges.push_back(Edge(2, 1));
					edges.push_back(Edge(2, 3));
					edges.push_back(Edge(2, 4));
				}
				else if(topleft == bottomleft)
				{
					edges.push_back(Edge(2, 0));
					edges.push_back(Edge(2, 3));
					edges.push_back(Edge(2, 4));
				}
				else if(topleft == bottomright)
				{
					edges.push_back(Edge(0, 3));
					edges.push_back(Edge(1, 4));
				}
				else if(topright == bottomleft)
				{
					edges.push_back(Edge(0, 1));
					edges.push_back(Edge(3, 4));
				}
				else if(topright == bottomright)
				{
					edges.push_back(Edge(2, 0));
					edges.push_back(Edge(2, 1));
					edges.push_back(Edge(2, 4));
				}
				else	// bottomleft == bottomright
				{
					edges.push_back(Edge(2, 0));
					edges.push_back(Edge(2, 1));
					edges.push_back(Edge(2, 3));
				}
				break;
			}
			default:
			{
				edges.push_back(Edge(2, 0));
				edges.push_back(Edge(2, 1));
				edges.push_back(Edge(2, 3));
				edges.push_back(Edge(2, 4));
				break;
			}
		}

		return edges;
	}

	void execute_impl()
	{
		m_labelling = m_data->labelling();

		Label topleft, topright, bottomleft, bottomright;

		// Look up the labels of the face corners.
		switch(m_faceDesignator)
		{
			case CubeFaceDesignator::FACE_XY:
			{
				topleft = label(m_x,m_y+1,m_z);		topright = label(m_x+1,m_y+1,m_z);
				bottomleft = label(m_x,m_y,m_z);	bottomright = label(m_x+1,m_y,m_z);
				break;
			}
			case CubeFaceDesignator::FACE_XZ:
			{
				topleft = label(m_x,m_y,m_z+1);		topright = label(m_x+1,m_y,m_z+1);
				bottomleft = label(m_x,m_y,m_z);	bottomright = label(m_x+1,m_y,m_z);
				break;
			}
			case CubeFaceDesignator::FACE_YZ:
			{
				topleft = label(m_x,m_y,m_z+1);		topright = label(m_x,m_y+1,m_z+1);
				bottomleft = label(m_x,m_y,m_z);	bottomright = label(m_x,m_y+1,m_z);
				break;
			}
			default:
			{
				throw Exception("Invalid face designator");		// this should never happen
			}
		}

		// Calculate the edges on the face from the labels of the corners.
		std::list<Edge> edges = edges_on_face(topleft, topright, bottomleft, bottomright);

		// If there aren't any edges, this cube face is irrelevant to the mesh.
		if(edges.size() == 0) return;

		// Construct the cube face, marking each used local node to indicate that we need to look up its global node.
		CubeFace cubeFace;
		for(std::list<Edge>::const_iterator it=edges.begin(), iend=edges.end(); it!=iend; ++it)
		{
			cubeFace.set_used(CubeFace::FaceNodeDesignator(it->u));
			cubeFace.set_used(CubeFace::FaceNodeDesignator(it->v));
		}

		// TODO
	}

	Label label(int x, int y, int z) const
	{
		itk::Index<3> index = {{x,y,z}};
		return m_labelling->GetPixel(index);
	}
};

}

#endif
