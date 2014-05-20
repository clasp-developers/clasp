#ifndef	SimulatedAnnealingSearch_H //[
#define SimulatedAnnealingSearch_H



#include <stdio.h>
#include <string>
#include <vector>
#include <set>
#include "foundation.h"
#include "object.h"
#include "lisp.h"
#include "holder.h"
#include "search.h"



namespace mbb
{


    DECLARE_PREDEFINED_SYMBOL(_sym_MbbPackage_simulatedAnnealingSearchTypeConverter);

    typedef enum
    {
	sequenceWithBestConformation,
	bestConformationOfOneSequence
    } SimulatedAnnealingSearchType;

    DECLARE_ENUM_SYMBOL_TRANSLATOR(SimulatedAnnealingSearchType,_sym_MbbPackage_simulatedAnnealingSearchTypeConverter);



    SMART(BuilderState);
    SMART(PosixTime);
    SMART(PosixTimeDuration);
    SMART(SearchStatistics);
    SMART(Builder);
    SMART(ScorerState);




    class ScorePoint : public LispObject
    {
    public:
    ScorePoint(const RPLisp& lisp) : LispObject(lisp) {};
	friend class OneTemperatureScoreSeries;
	friend class O_ScoreLogger;
    public:
    	double	_Score;
	bool	_Accepted;

	RPCons asCons(RPLisp env);
	void archive(RPNode node);
    };

    class	OneTemperatureScoreSeries : public LispObject
    {
    public:
    OneTemperatureScoreSeries(const RPLisp& lisp) : LispObject(lisp) {};
	friend class O_ScoreLogger;
    public:
	double		_Temperature;
	vector<ScorePoint>	_Scores;
    public:
	/*! Return a cons of elements each a cons containing
	 * the score followed by if it was accepted or not
	 */
	RPCons asCons(RPLisp);

	void archive(RPNode node);
    };


/*!
 * Log SimulatedAnnealing temperature/score/accepted progress
 */
    __BEGIN_CLASS_DEFINITION(MbbPackage,O_ScoreLogger,ScoreLogger,O_Object)
	public: // virtual functions inherited from Object
    void	initialize();
//	void	oldLispInitialize(RPKeyedArguments kargs, RPLisp);
    void	archiveBase(RPNode node);
//	string	__repr__() const;

private: // instance variables
    vector<OneTemperatureScoreSeries>	_TemperatureScoreSeries;
    RPPosixTime				_CreationTime;
    RPPosixTime				_LastUpdateTime;

public:	// Creation class functions
public:
    void newTemperature(double temperature);
    void appendScore(double score, bool accepted );
    RPCons asCons();
public:
    O_ScoreLogger( const O_ScoreLogger& ss ); //!< Copy constructor

    RPPosixTimeDuration runDuration();

    __END_CLASS_DEFINITION(O_ScoreLogger)


/*!
 * Log SimulatedAnnealing temperature/score/accepted progress
 */
	class O_ScoreLoggerList : public O_Object
    {
	BEGIN_LISP_CLASS(MbbPackage,O_ScoreLoggerList,ScoreLoggerList,"O_Object")
	    public:
	static void lisp_initGlobals(RPLisp lisp);
	static void exposeCando(RPLisp lisp);
	static void exposePython();
    public: // virtual functions inherited from Object
	void	initialize();
	void	oldLispInitialize(RPKeyedArguments kargs," RPLisp");
	void	archiveBase(RPNode node);
//	string	__repr__() const;

    private: // instance variables
	List<O_ScoreLogger>	_ScoreLoggers;

    public:	// Creation class functions
    public:
	void append(RPScoreLogger);
	RPCons asCons();
    public:
//	O_ScoreLoggerList( const O_ScoreLogger& ss ); //!< Copy constructor

	END_LISP_CLASS(O_ScoreLoggerList)
	    };

    SMART(SimulatedAnnealingSearch);
    __BEGIN_CLASS_DEFINITION(MbbPackage,O_SimulatedAnnealingSearch,SimulatedAnnealingSearch,O_Search) // {
	public:
    static void exposeCando(RPLisp lisp);
    static void exposePython() { IMPLEMENT_ME();};
    static void lisp_initGlobals(RPLisp lisp);
    void	oldLispInitialize(RPKeyedArguments kargs, RPLisp);
    void	initialize();
private:
    bool	_UseRandomConformations;
    //! Number of steps used to estimate temperature
    int	_NumberOfTemperatureEstimationSteps;
    double	_ScaleInitialTemperatureEstimate;
    //! If this is zero then generate ALL conformations
    int	_NumberOfConformationsPerSequence;
    int	_NumberOfTemperatureSteps;
    int	_NumberOfStepsPerTemperature;
    SimulatedAnnealingSearchType _SearchType;
    //! If you want the program to estimate the initial temperature use this
    bool	_EstimateInitialTemperature;
    /*! If you want to define the initial temperature,
     * _EstimateInitialTemperature will be set to false
     */
    double	_InitialTemperature;
    /*! How much to scale the temperature for each temperature scale
     */
    double	_TemperatureStepScale;
    uint	_MaxPerturbationsToFindStartingPoint;
    uint	_MaxRejectsPerTemperatureEstimationStep;
    //! The probability that the oligomer will change when taking a step
    double	_PerturbOligomerProbability;
    bool	_ShowProgress;
    RPScoreLogger	_ScoreLogger;
private:	// Things that are calculated while the MCMC runs
    int	_Accepts;
    bool	_TookInitialStep;
    double	_CurrentScore;
    uint	_InitialTemperatureBuilds;
    uint	_InitialTemperatureRejects;
    LongLongInt	_NumberOfConformationsBuilt;

public:
    static RPObject search(RPCons exp, RPLisp);
    static RPObject prim_simulatedAnnealingSearchForSequenceWithBestConformation(RPExecutable e, RPCons args, RPEnvironment environ, RPLisp lisp);
    static RPObject prim_simulatedAnnealingSearchForBestConformationOfOneSequence(RPExecutable e, RPCons args, RPEnvironment environ, RPLisp lisp);

private:
    double	_estimateInitialTemperature(RPSearchStatistics stats );
    void perturbSequence(RPGenerator);
    void searchForBestConformation(RPGenerator , RPBuilderState bestBuilderState,
				   RPScorerState bestScorerState );
public:


    void	setupScoreLogger();
    RPScoreLogger getScoreLogger();


    /*! Call this before you estimate initial temperature,
     * perturb the state or call run
     */
    void	takeInitialStep();

    /*! Perturbing the state and evaluate the
     * score until you find a non-rejected starting point
     * or you carry out _MaxPerturbationsToFindStartingPoint
     */
    bool findValidStartingPoint(RPSearchStatistics statistics);

    /*! Perturb the state and return its score
     * The generator will have the same state 
     * stored in bestBuildreState and bestScorerState
     */
    void perturbStateAndScore(RPGenerator bs, RPBuilderState bestBuilderState, RPScorerState bestScorerState );

    /*! Check if the state is accepted, return true if it is
     */
    bool checkAccept(RPScorerState, double temperature, double currentScore );

    double	estimateInitialTemperature(RPSearchStatistics stats );
	
    virtual void run();

    string description() const;

    void setSearchType(SimulatedAnnealingSearchType t) { this->_SearchType = t;};
    void setDefaultOptions();
    void setKeyedOptions(RPKeyedArguments options);
    void setOptions(RPCons options);

    O_SimulatedAnnealingSearch( const O_SimulatedAnnealingSearch& ss ); //!< Copy constructor


    __END_CLASS_DEFINITION(O_SimulatedAnnealingSearch) //}



	};
#endif //]
