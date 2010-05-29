/***
 * millipede: IPFGrid.h
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_IPFGRID
#define H_MILLIPEDE_IPFGRID

#include <cassert>
#include <vector>

#include <itkSize.h>

#include <common/partitionforests/base/PartitionForest.h>

namespace mp {

template <typename IPF>
class IPFGrid
{
	//#################### TYPEDEFS ####################
public:
	typedef boost::shared_ptr<IPF> IPF_Ptr;
	typedef boost::shared_ptr<const IPF> IPF_CPtr;

	//#################### PRIVATE VARIABLES ####################
private:
	std::vector<IPF_Ptr> m_forests;
	itk::Size<3> m_subvolumeSize;
	itk::Size<3> m_volumeSize;

	//#################### CONSTRUCTORS ####################
public:
	IPFGrid(const std::vector<IPF_Ptr>& forests, const itk::Size<3>& subvolumeSize, const itk::Size<3>& volumeSize)
	:	m_forests(forests), m_subvolumeSize(subvolumeSize), m_volumeSize(volumeSize)
	{}

	//#################### PUBLIC METHODS ####################
public:
	int forest_count() const
	{
		return static_cast<int>(m_forests.size());
	}

	IPF_Ptr forest(int n)
	{
		assert(0 <= n && n < forest_count());
		return m_forests[n];
	}

	IPF_CPtr forest(int n) const
	{
		assert(0 <= n && n < forest_count());
		return m_forests[n];
	}

	int forest_index_of(int x, int y, int z) const
	{
		int subvolumeX = x / m_subvolumeSize[0];
		int subvolumeY = y / m_subvolumeSize[1];
		int subvolumeZ = z / m_subvolumeSize[2];
		size_t index = (subvolumeZ * m_subvolumeSize[1] + subvolumeY) * m_subvolumeSize[0] + subvolumeX;
		assert(0 <= index && index < m_forests.size());
		return index;
	}

	IPF_Ptr forest_of(int x, int y, int z)
	{
		return m_forests[forest_index_of(x,y,z)];
	}

	IPF_CPtr forest_of(int x, int y, int z) const
	{
		return m_forests[forest_index_of(x,y,z)];
	}

	int leaf_index_of(int x, int y, int z) const
	{
		int offsetX = x % m_subvolumeSize[0];
		int offsetY = y % m_subvolumeSize[1];
		int offsetZ = z % m_subvolumeSize[2];
		return (offsetZ * m_subvolumeSize[1] + offsetY) * m_subvolumeSize[0] + offsetX;
	}

	const itk::Size<3>& subvolume_size() const
	{
		return m_subvolumeSize;
	}

	const itk::Size<3>& volume_size() const
	{
		return m_volumeSize;
	}
};

}

#endif
