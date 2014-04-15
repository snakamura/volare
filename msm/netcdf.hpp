#include <tr1/array>
#include <exception>
#include <string>
#include <netcdf.h>

namespace netcdf {

class File;
class Var;
class Exception;
    
class File
{
public:
    File(const std::string& path);
    ~File();

public:
    Var var(const std::string& name) const;

private:
    int ncId_;
};

    
class Var
{
public:
    Var(int ncId, int varId);

public:
    double attDouble(const std::string& name) const;

    template<size_t n>
    void get(const std::tr1::array<size_t, n>& start, const std::tr1::array<size_t, n>& count, short* value) const;

    template<size_t n>
    void get(const std::tr1::array<size_t, n>& start, const std::tr1::array<size_t, n>& count, double* value) const;

private:
    int ncId_;
    int varId_;
};


class Exception : std::exception
{
public:
    Exception(int status, const std::string& message);

public:
    int status() const;
    std::string message() const;

private:
    int status_;
    std::string message_;
};


static inline void handleError(int status, const std::string& message) {
    if (status != NC_NOERR) {
        throw Exception(status, message);
    }
}

}


template<size_t n>
void netcdf::Var::get(const std::tr1::array<size_t, n>& start, const std::tr1::array<size_t, n>& count, short* value) const {
    handleError(nc_get_vara_short(ncId_, varId_, start.data(), count.data(), value), "nc_get_vara_short failed");
}


template<size_t n>
void netcdf::Var::get(const std::tr1::array<size_t, n>& start, const std::tr1::array<size_t, n>& count, double* value) const {
    handleError(nc_get_vara_double(ncId_, varId_, start.data(), count.data(), value), "nc_get_vara_double failed");
}
