using namespace std;
using namespace Rcpp;

using Eigen::Map;
using Eigen::MatrixXd;
using Eigen::SparseMatrix;
using Eigen::VectorXd;
using Rcpp::as;

template <typename T> int sign(T val);

void Cox_Refresh_R_TERM(const int& totalnum, const int& reqrdnum, const int& term_tot, double& dint, double& dslp,double& dose_abs_max, double& abs_max, const MatrixXd& df0, MatrixXd& T0, MatrixXd& Td0, MatrixXd& Tdd0, MatrixXd& Te, MatrixXd& R, MatrixXd& Rd, MatrixXd& Rdd, MatrixXd& Dose, MatrixXd& nonDose,  MatrixXd& TTerm,  MatrixXd& nonDose_LIN, MatrixXd& nonDose_PLIN, MatrixXd& nonDose_LOGLIN, MatrixXd& RdR, MatrixXd& RddR, bool basic_bool, bool single_bool);

void Cox_Refresh_R_SIDES( const int& reqrdnum, const int& ntime, MatrixXd& Rls1, MatrixXd& Rls2, MatrixXd& Rls3, MatrixXd& Lls1, MatrixXd& Lls2, MatrixXd& Lls3, NumericVector& STRATA_vals, bool strata_bool, bool single_boo);

void Cox_Term_Risk_Calc(string modelform, const StringVector& tform, const IntegerVector& term_n, const int& totalnum, const int& fir, const IntegerVector& dfc, int term_tot, MatrixXd& T0, MatrixXd& Td0, MatrixXd& Tdd0, MatrixXd& Te, MatrixXd& R, MatrixXd& Rd, MatrixXd& Rdd, MatrixXd& Dose, MatrixXd& nonDose, VectorXd beta_0,const  MatrixXd& df0,const double& dint, const double& dslp,  MatrixXd& TTerm,  MatrixXd& nonDose_LIN, MatrixXd& nonDose_PLIN, MatrixXd& nonDose_LOGLIN, MatrixXd& RdR, MatrixXd& RddR, const int& nthreads, bool debugging, const IntegerVector& KeepConstant, int verbose, bool basic_bool, bool single_bool, int start, const double gmix_theta, const IntegerVector& gmix_term);

void Cox_Side_LL_Calc(const int& reqrdnum, const int& ntime, const IntegerMatrix& RiskFail, const StringMatrix&  RiskGroup_Strata, const vector<string>& RiskGroup, const int& totalnum, const int& fir, MatrixXd& R, MatrixXd& Rd, MatrixXd& Rdd,  MatrixXd& Rls1, MatrixXd& Rls2, MatrixXd& Rls3, MatrixXd& Lls1, MatrixXd& Lls2, MatrixXd& Lls3, const VectorXd& cens_weight, NumericVector& STRATA_vals, VectorXd beta_0 , MatrixXd& RdR, MatrixXd& RddR, vector<double>& Ll, vector<double>& Lld, vector<double>& Lldd, const int& nthreads, bool debugging, const IntegerVector& KeepConstant,string ties_method, int verbose,bool strata_bool, bool CR_bool, bool basic_bool, bool single_bool, int start, int iter_stop);

void Pois_Term_Risk_Calc(string modelform, const StringVector& tform, const IntegerVector& term_n, const int& totalnum, const int& fir, const IntegerVector& dfc, int term_tot, MatrixXd& T0, MatrixXd& Td0, MatrixXd& Tdd0, MatrixXd& Te, MatrixXd& R, MatrixXd& Rd, MatrixXd& Rdd, MatrixXd& Dose, MatrixXd& nonDose, VectorXd beta_0,const  MatrixXd& df0,const double& dint, const double& dslp,  MatrixXd& TTerm,  MatrixXd& nonDose_LIN, MatrixXd& nonDose_PLIN, MatrixXd& nonDose_LOGLIN, MatrixXd& RdR, MatrixXd& RddR, const MatrixXd& s_weights, const int& nthreads, bool debugging, const IntegerVector& KeepConstant, int verbose, bool strata_bool, bool single_bool, int start, const double gmix_theta, const IntegerVector& gmix_term);

void Pois_Dev_LL_Calc(const int& reqrdnum, const int& totalnum, const int& fir, MatrixXd& R, MatrixXd& Rd, MatrixXd& Rdd, VectorXd beta_0 , MatrixXd& RdR, MatrixXd& RddR, vector<double>& Ll, vector<double>& Lld, vector<double>& Lldd, const MatrixXd& PyrC, MatrixXd& dev_temp, const int& nthreads, bool debugging, const IntegerVector& KeepConstant, int verbose, bool single_bool, int start, int iter_stop, double& dev);

void Cox_Pois_Check_Continue(const bool basic_bool, VectorXd beta_0, vector<double>& beta_best, vector<double>& beta_c, const VectorXd& cens_weight, const bool change_all, const bool cox_bool, const bool CR_bool, vector<double>& dbeta, const bool debugging, double& dev, MatrixXd& dev_temp, const int fir, const int halfmax, double& halves, int& ind0, int& iter_stop, const IntegerVector& KeepConstant, vector<double>& Ll, double& Ll_abs_best, vector<double>& Lld, vector<double>& Lldd, MatrixXd& Lls1, MatrixXd& Lls2, MatrixXd& Lls3, const bool log_bound_bool, const double& Lstar, const int& nthreads, const int& ntime, const MatrixXd& PyrC, MatrixXd& R, MatrixXd& Rd, MatrixXd& Rdd, MatrixXd& RddR, MatrixXd& RdR, const int& reqrdnum, const IntegerMatrix& RiskFail, const vector<string>& RiskGroup, const StringMatrix& RiskGroup_Strata, MatrixXd& Rls1, MatrixXd& Rls2, MatrixXd& Rls3, const bool single_bool, int start, const bool strata_bool, NumericVector& STRATA_vals, const IntegerVector& term_n, const string ties_method, const int totalnum, MatrixXd& TTerm, const int verbose);

void Cox_Pois_Log_Loop(double& abs_max, const bool basic_bool, VectorXd beta_0, vector<double>& beta_a, vector<double>& beta_c, int& bound_val, const bool cox_bool, vector<double>& dbeta, const bool debugging, const MatrixXd& df0, IntegerVector& dfc, double& dint, MatrixXd& Dose, double& dose_abs_max, double& dslp, const int fir, const IntegerVector& gmix_term, const double& gmix_theta, int& half_check,const int halfmax, const IntegerVector& KeepConstant, vector<bool>& limit_hit, double& lr, string& modelform, MatrixXd& nonDose, MatrixXd& nonDose_LIN, MatrixXd& nonDose_LOGLIN, MatrixXd& nonDose_PLIN, const int& nthreads, MatrixXd& R, MatrixXd& Rd, MatrixXd& Rdd, MatrixXd& RddR, MatrixXd& RdR, VectorXd& s_weights, const bool single_bool, int start, const bool strata_bool, MatrixXd& T0, MatrixXd& Td0, MatrixXd& Tdd0, MatrixXd& Te, const IntegerVector& term_n, int& term_tot, StringVector& tform, const int totalnum, MatrixXd& TTerm, const int verbose);
