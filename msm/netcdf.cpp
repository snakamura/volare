#include <netcdf.h>
#include "netcdf.hpp"

using namespace std;
using namespace netcdf;


netcdf::File::File(const string& path) :
    ncId_(-1) {
    handleError(nc_open(path.c_str(), NC_NOWRITE, &ncId_), "nc_open failed");
}

netcdf::File::~File() {
    if (ncId_ != -1) {
        nc_close(ncId_);
    }
}

Var netcdf::File::var(const string& name) const {
    int varId = -1;
    handleError(nc_inq_varid(ncId_, name.c_str(), &varId), "nc_inq_varid failed: " + name);
    return Var(ncId_, varId);
}


netcdf::Var::Var(int ncId, int varId) :
    ncId_(ncId),
    varId_(varId) {
}

double netcdf::Var::attDouble(const string& name) const {
    double d = 0.0;
    handleError(nc_get_att_double(ncId_, varId_, name.c_str(), &d), "nc_var_att_double failed: " + name);
    return d;
}


netcdf::Exception::Exception(int status, const string& message) :
    status_(status),
    message_(message) {
}

int netcdf::Exception::status() const {
    return status_;
}

string netcdf::Exception::message() const {
    return message_;
}
