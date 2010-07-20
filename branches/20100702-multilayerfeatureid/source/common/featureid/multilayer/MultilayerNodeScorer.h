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
public:
	typedef std::map<PFNodeID,double> Scores;
protected:
	typedef typename BranchLayer::NodeProperties BranchProperties;
	typedef PartitionForest<LeafLayer,BranchLayer> PartitionForestT;
	typedef boost::shared_ptr<const PartitionForestT> PartitionForest_CPtr;

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
	virtual double calculate_score(const BranchProperties& properties) const = 0;

	//#################### PUBLIC METHODS ####################
public:
	void iterate()
	{
		int highestLayer = m_forest->highest_layer();
		if(highestLayer == 0) return;	// it's possible to have a forest with only a leaf layer

		typedef typename BranchLayer::BranchNodeConstIterator Iter;
		for(Iter it=m_forest->branch_nodes_cbegin(highestLayer), iend=m_forest->branch_nodes_cend(highestLayer); it!=iend; ++it)
		{
			PFNodeID node(highestLayer, it.index());
			propagate_scores_up(node);
			normalize_scores();
			propagate_scores_down(node);
			normalize_scores();
		}
	}

	void reset()
	{
		m_scores.clear();
	}

	const Scores& scores() const
	{
		return m_scores;
	}

	//#################### PROTECTED METHODS ####################
protected:
	static double gaussian_minmax(double x, double min, double max)
	{
		return gaussian_musigma(x, (min+max)/2, max - min);
	}

	static double gaussian_musigma(double x, double mu, double sigma)
	{
		// TODO: This should be placed somewhere more central.
		const double PI = 3.141592654;

		double offset = x - mu;
		double denom = 2*sigma*sigma;
		double numer = -offset*offset;
		return 1/sqrt(PI*denom) * exp(numer/denom);
	}
	
	//#################### PRIVATE METHODS ####################
private:
	void normalize_scores()
	{	
		double max_score = 0;
		int highest_layer = m_forest->highest_layer(); 
		typedef typename BranchLayer::BranchNodeConstIterator Iter;
		for(Iter it=m_forest->branch_nodes_cbegin( highest_layer), iend=m_forest->branch_nodes_cend(highest_layer); it!=iend; ++it)
		{
			PFNodeID node(highest_layer, it.index());
			double score  = m_scores[node];
			max_score = std::max(max_score, score);
		}
		if (max_score == 0){return;}
		for(Iter it=m_forest->branch_nodes_cbegin( highest_layer), iend=m_forest->branch_nodes_cend(highest_layer); it!=iend; ++it)
		{
			PFNodeID node(highest_layer, it.index());
			adjust_scores(node,1 / max_score);
		}
	}

	void adjust_scores(const PFNodeID& node, double factor)
	{
		m_scores[node] = m_scores[node] * factor;
		std::set<PFNodeID> children = m_forest->children_of(node);
		for(std::set<PFNodeID>::const_iterator it=children.begin(), iend=children.end(); it!=iend; ++it)
		{
			adjust_scores(*it,factor);
		}
	}
	
	double max_score(const PFNodeID& node)
	{
		double score = m_scores[node];
		std::set<PFNodeID> children = m_forest->children_of(node);
		for(std::set<PFNodeID>::const_iterator it=children.begin(), iend=children.end(); it!=iend; ++it)
		{
			score = std::max(score, max_score(*it));
		}
		return score;            
	}
	
	void propagate_scores_down(const PFNodeID& cur, double parentScore = -1.0)
	{
		double& score = m_scores[cur];
		if(parentScore > 0.0){
			score = sqrt(score * parentScore);
		}

		if(cur.layer() > 1)
		{
			std::set<PFNodeID> children = m_forest->children_of(cur);
			for(std::set<PFNodeID>::const_iterator it=children.begin(), iend=children.end(); it!=iend; ++it)
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
			result = calculate_score(m_forest->branch_properties(cur));
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
