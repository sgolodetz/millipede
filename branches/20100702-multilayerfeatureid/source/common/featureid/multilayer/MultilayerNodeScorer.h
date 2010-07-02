/***
 * millipede: MultilayerNodeScorer.h
 * Copyright Stuart Golodetz & Chris Nicholls, 2010. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_MULTILAYERNODESCORER
#define H_MILLIPEDE_MULTILAYERNODESCORER

#include <cassert>
#include <cmath>
#include <map>

#include <boost/shared_ptr.hpp>

#include <common/partitionforests/base/PartitionForest.h>
#include <common/partitionforests/base/PFNodeID.h>

namespace mp {

template <typename LeafLayer, typename BranchLayer>
class MultilayerNodeScorer
{
	//#################### TYPEDEFS ####################
private:
	typedef PartitionForest<LeafLayer,BranchLayer> PartitionForestT;
	typedef boost::shared_ptr<const PartitionForestT> PartitionForest_CPtr;
	typedef std::map<PFNodeID,double> Scores;

	//#################### PRIVATE VARIABLES ####################
private:
	PartitionForest_CPtr m_forest;
	Scores m_scores;

	//#################### CONSTRUCTORS ####################
public:
	explicit MultilayerNodeScorer(const PartitionForest_CPtr& forest)
	:	m_forest(forest)
	{}

	//#################### DESTRUCTOR ####################
public:
	virtual ~MultilayerNodeScorer()
	{}

	//#################### PRIVATE ABSTRACT METHODS ####################
private:
	virtual double calculate_score(const PFNodeID& node) const = 0;

	//#################### PUBLIC METHODS ####################
public:
	void iterate()
	{
		int highestLayer = m_forest->highest_layer();
		if(highestLayer == 0) return;	// it's possible to have a forest with only a leaf layer

		typedef typename BranchLayer::BranchNodeConstIterator Iter;
		for(Iter it=m_forest->branch_nodes_cbegin(highestLayer), iend=m_forest->branch_nodes_cend(highestLayer); it!=iend; ++it)
		{
			propagate_scores_up(*it);
			propagate_scores_down(*it);
		}
	}

	const Scores& scores() const
	{
		return m_scores;
	}
	
	//#################### PRIVATE METHODS ####################
private:
	void propagate_scores_down(const PFNodeID& cur, double parentScore = -1.0)
	{
		double& score = m_scores[cur];
		if(parentScore >= 0.0) score = sqrt(score * parentScore);

		if(cur.layer() > 1)
		{
			std::set<PFNodeID> children = m_forest->children_of(cur);
			for(std::list<PFNodeID>::const_iterator it=children.begin(), iend=children.end(); it!=iend; ++it)
			{
				propagate_scores_down(*it, score);
			}
		}
	}

	double propagate_scores_up(const PFNodeID& cur)
	{
		double result;
		if(cur.layer() == 1)
		{
			// If the current node's in the lowest branch layer, calculate its score explicitly.
			result = calculate_score(cur);
		}
		else
		{
			// Otherwise, calculate the scores of its children recursively, and propagate them upwards.
			std::set<PFNodeID> children = m_forest->children_of(cur);
			assert(!children.empty());	// a branch node with no children would be invalid
			int areaSum = 0;
			double weightedScoreSum = 0;
			for(std::set<PFNodeID>::const_iterator it=children.begin(), iend=children.end(); it!=iend; ++it)
			{
				int area = m_forest->branch_properties(*it).area();
				double score = propagate_scores_up(*it);
				areaSum += area;
				weightedScoreSum += area * score;
			}
			result = weightedScoreSum / areaSum;
		}

		m_scores[cur] = result;
		return result;
	}
};

}

#endif
