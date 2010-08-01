/***
 * millipede: PartitionForestMFSManager.h
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_PARTITIONFORESTMFSMANAGER
#define H_MILLIPEDE_PARTITIONFORESTMFSMANAGER

#include "PartitionForestMultiFeatureSelection.h"

namespace mp {

template <typename MFS>
class PartitionForestMFSManager
{
	//#################### TYPEDEFS ####################
private:
	typedef boost::shared_ptr<MFS> MFS_Ptr;
	typedef boost::shared_ptr<const MFS> MFS_CPtr;
public:
	typedef std::map<std::string,MFS_Ptr> MFSMap;

	//#################### COMMANDS ####################
private:
	struct AddMFSCommand : Command
	{
		PartitionForestMFSManager *m_base;
		std::string m_name;
		MFS_Ptr m_mfs;

		AddMFSCommand(PartitionForestMFSManager *base, const std::string& name, const MFS_Ptr& mfs)
		:	Command("Add Multi-Feature Selection"), m_base(base), m_name(name), m_mfs(mfs)
		{}

		void execute()
		{
			m_base->m_multiFeatureSelections.insert(std::make_pair(m_name, m_mfs));
			m_base->m_listeners.multi_feature_selection_manager_changed();
		}

		void undo()
		{
			m_base->m_multiFeatureSelections.erase(m_name);
			m_base->m_listeners.multi_feature_selection_manager_changed();
		}
	};

	struct RemoveMFSCommand : Command
	{
		PartitionForestMFSManager *m_base;
		std::string m_name;
		MFS_Ptr m_oldMFS;
		bool m_oldMFSWasActive;

		RemoveMFSCommand(PartitionForestMFSManager *base, const std::string& name)
		:	Command("Remove Multi-Feature Selection"), m_base(base), m_name(name)
		{}

		void execute()
		{
			m_oldMFS = m_base->m_multiFeatureSelections.find(m_name)->second;
			m_oldMFSWasActive = m_base->m_activeMultiFeatureSelection.first == m_name;
			m_base->m_multiFeatureSelections.erase(m_name);
			if(m_oldMFSWasActive) m_base->m_activeMultiFeatureSelection = *m_base->m_multiFeatureSelections.begin();
			m_base->m_listeners.multi_feature_selection_manager_changed();
		}

		void undo()
		{
			std::pair<std::string,MFS_Ptr> p = std::make_pair(m_name, m_oldMFS);
			m_base->m_multiFeatureSelections.insert(p);
			if(m_oldMFSWasActive) m_base->m_activeMultiFeatureSelection = p;
			m_base->m_listeners.multi_feature_selection_manager_changed();
		}
	};

	struct SetActiveMFSCommand : Command
	{
		PartitionForestMFSManager *m_base;
		std::string m_name;
		boost::optional<std::pair<std::string,MFS_Ptr> > m_oldActiveMultiFeatureSelection;

		SetActiveMFSCommand(PartitionForestMFSManager *base, const std::string& name)
		:	Command("Set Active Feature Selection"), m_base(base), m_name(name)
		{}

		void execute()
		{
			typename MFSMap::iterator it = m_base->m_multiFeatureSelections.find(m_name);
			if(it != m_base->m_multiFeatureSelections.end())
			{
				m_oldActiveMultiFeatureSelection = m_base->m_activeMultiFeatureSelection;
				m_base->m_activeMultiFeatureSelection = *it;
				m_base->m_listeners.multi_feature_selection_manager_changed();
			}
			else throw Exception("Multi-feature selection " + m_name + " does not exist");
		}

		void undo()
		{
			m_base->m_activeMultiFeatureSelection = *m_oldActiveMultiFeatureSelection;
			m_oldActiveMultiFeatureSelection.reset();
			m_base->m_listeners.multi_feature_selection_manager_changed();
		}
	};

	//#################### LISTENERS ####################
public:
	struct Listener
	{
		virtual ~Listener() {}
		virtual void multi_feature_selection_manager_changed() = 0;
	};

private:
	struct CompositeListener : CompositeListenerBase<Listener>
	{
		void multi_feature_selection_manager_changed()
		{
			multicast(boost::bind(&Listener::multi_feature_selection_manager_changed, _1));
		}
	};

	//#################### PRIVATE VARIABLES ####################
private:
	std::pair<std::string,MFS_Ptr> m_activeMultiFeatureSelection;
	ICommandManager_Ptr m_commandManager;
	CompositeListener m_listeners;
	MFSMap m_multiFeatureSelections;

	//#################### CONSTRUCTORS ####################
public:
	PartitionForestMFSManager(const std::string& initialMFSName, const MFS_Ptr& initialMFS)
	:	m_activeMultiFeatureSelection(initialMFSName, initialMFS), m_commandManager(new BasicCommandManager)
	{
		m_multiFeatureSelections.insert(std::make_pair(initialMFSName, initialMFS));
	}

	//#################### PUBLIC METHODS ####################
public:
	const MFS_Ptr& active_multi_feature_selection()
	{
		return m_activeMultiFeatureSelection.second;
	}

	MFS_CPtr active_multi_feature_selection() const
	{
		return m_activeMultiFeatureSelection.second;
	}

	void add_multi_feature_selection(const std::string& name, const MFS_Ptr& mfs)
	{
		mfs->set_command_manager(m_commandManager);
		m_commandManager->execute(Command_Ptr(new AddMFSCommand(this, name, mfs)));
	}

	void add_shared_listener(const boost::shared_ptr<Listener>& listener)
	{
		m_listeners.add_shared_listener(listener);
	}

	void add_weak_listener(const boost::weak_ptr<Listener>& listener)
	{
		m_listeners.add_weak_listener(listener);
	}

	bool has_multi_feature_selection(const std::string& name) const
	{
		return m_multiFeatureSelections.find(name) != m_multiFeatureSelections.end();
	}

	const MFSMap& multi_feature_selections() const
	{
		return m_multiFeatureSelections;
	}

	void remove_multi_feature_selection(const std::string& name)
	{
		m_commandManager->execute(Command_Ptr(new RemoveMFSCommand(this, name)));
	}

	void set_active_multi_feature_selection(const std::string& name)
	{
		m_commandManager->execute(Command_Ptr(new SetActiveMFSCommand(this, name)));
	}

	void set_command_manager(const ICommandManager_Ptr& commandManager)
	{
		m_commandManager = commandManager;
		for(typename MFSMap::iterator it=m_multiFeatureSelections.begin(), iend=m_multiFeatureSelections.end(); it!=iend; ++it)
		{
			it->second->set_command_manager(commandManager);
		}
	}
};

}

#endif
