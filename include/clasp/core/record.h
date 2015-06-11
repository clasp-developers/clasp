#ifndef core_record_H
#define core_record_H

namespace core {

  T_sp record_circle_subst(T_sp repl_table, T_sp tree );

  
  SMART(Record);
  class Record_O : public T_O {
    LISP_BASE1(T_O);
    LISP_VIRTUAL_CLASS(core, CorePkg, Record_O, "Record");
  public:
    typedef enum {loading, saving, patching } RecordStage;
  public:
    RecordStage _stage;
    List_sp _alist;
    T_sp _replacement_table;
  public: // Simple default ctor/dtor
  Record_O() : _stage(saving), _alist(_Nil<T_O>()) {};
  Record_O(RecordStage stage, List_sp data) : _stage(stage), _alist(data) {};
  Record_O(RecordStage stage, T_sp replacement_table, bool dummy) : _stage(stage), _replacement_table(replacement_table) {};
    virtual ~Record_O() {};
  public:
    static Record_sp create_encoder() {
      Record_sp record = gctools::GCObjectAllocator<Record_O>::allocate(saving,_Nil<T_O>());
      return record;
    }
    static Record_sp create_decoder(List_sp data) {
      Record_sp record = gctools::GCObjectAllocator<Record_O>::allocate(loading,data);
      return record;
    }
    static Record_sp create_patcher(HashTable_sp replacement_table)
    {
      Record_sp record = gctools::GCObjectAllocator<Record_O>::allocate(patching,replacement_table,true);
      return record;
    }
   
  public:
    List_sp data() const { return this->_alist; };
    RecordStage stage() const { return this->_stage; };

    template <typename ST>
      void field(Symbol_sp name, ST& value ) {
      switch (this->stage()) {
      case saving: {
          this->_alist = core::Cons_O::create(core::Cons_O::create(name,translate::to_object<ST>::convert(value)),this->_alist);
      }
          break;
      case loading: {
      // I could speed this up if I cache the entry after this find
      // and search from there and reverse the alist once it's done
          List_sp find = alist_get(this->_alist,name);
          if ( find.nilp() ) SIMPLE_ERROR(BF("Could not find field %s") % _rep_(name));
          value = translate::from_object<ST>(oCdr(oCar(find)))._v;
        }
        break;
      case patching:
          // Do nothing for POD values
          break;
      };
    }

    template <typename OT>
      void field(Symbol_sp name, gc::smart_ptr<OT>& value ) {
      switch (this->stage()) {
      case saving:
          this->_alist = core::Cons_O::create(core::Cons_O::create(name,value),this->_alist);
          break;
      case loading: {
          List_sp find = alist_get(this->_alist,name);
          if ( find.nilp() ) SIMPLE_ERROR(BF("Could not find field %s") % _rep_(name));
          // Set the value and ignore its type!!!!!! This is to allow placeholders
          value.setRaw_(reinterpret_cast<gc::Tagged>(oCdr(oCar(find)).raw_()));
        }
        break;
      case patching:
          gc::smart_ptr<T_O> orig((gc::Tagged)value.raw_());
          T_sp patch = record_circle_subst(this->_replacement_table,orig);
          if ( patch != orig ) value.setRaw_(reinterpret_cast<gc::Tagged>(patch.raw_()));
          break;
      };
    }

    template <typename OT>
      void field(Symbol_sp name, gctools::Vec0<gc::smart_ptr<OT>>& value ) {
      switch (this->stage()) {
      case saving: {
          Vector_sp vec_value = core_make_vector(cl::_sym_T_O,value.size());
          size_t idx(0);
          for ( auto it : value ) vec_value->operator[](idx++) = it;
          this->_alist = core::Cons_O::create(core::Cons_O::create(name,vec_value),this->_alist);
        }
        break;
      case loading: {
      // I could speed this up if I cache the entry after this find
      // and search from there and reverse the alist once it's done
          List_sp find = alist_get(this->_alist,name);
          if ( find.nilp() ) SIMPLE_ERROR(BF("Could not find field %s") % _rep_(name));
          Vector_sp vec_value = gc::As<Vector_sp>(oCdr(oCar(find)));
          value.resize(cl_length(vec_value));
          for ( size_t i(0), iEnd(cl_length(vec_value)); i<iEnd; ++i ) {
            value[i] = (*vec_value)[i];
          }
        }
        break;
      case patching: {
          for ( size_t i(0), iEnd(value.size()); i<iEnd; ++i ) {
            gc::smart_ptr<T_O> orig((gc::Tagged)value[i].raw_());
            T_sp patch = record_circle_subst(this->_replacement_table,orig);
            if ( patch != orig ) value[i].rawRef_() = reinterpret_cast<OT*>(patch.raw_());
          }
        }
        break;
      }
    };

    template <typename OT>
      void field_if_not_nil(Symbol_sp name, gc::smart_ptr<OT>& value ) {
      switch ( this->stage() ) {
      case saving: {
          if ( value.notnilp() ) 
            this->_alist = Cons_O::create(Cons_O::create(name,value),this->_alist);
        }
        break;
      case loading: {
      // I could speed this up if I cache the entry after this find
      // and search from there and reverse the alist once it's done
          List_sp find = alist_get(this->_alist,name);
          if ( find.nilp() ) value = _Nil<core::T_O>();
          else value = gc::As<gc::smart_ptr<OT>>(oCdr(oCar(find)));
        }
        break;
      case patching: {
          if ( value.notnilp() ) {
            gc::smart_ptr<T_O> orig((gc::Tagged)value.raw_());
            T_sp patch = record_circle_subst(this->_replacement_table,orig);
            if ( patch != orig ) value.setRaw_(reinterpret_cast<gc::Tagged>(patch.raw_()));
          }
        }
        break;
      }
    }

    template <typename OT>
      void field_if_not_nil(Symbol_sp name, gc::Nilable<gc::smart_ptr<OT>>& value ) {
      switch ( this->stage() ) {
      case saving: {
          if ( value.notnilp() ) 
            this->_alist = Cons_O::create(Cons_O::create(name,value),this->_alist);
        }
        break;
      case loading: {
      // I could speed this up if I cache the entry after this find
      // and search from there and reverse the alist once it's done
          List_sp find = alist_get(this->_alist,name);
          if ( find.nilp() ) value = _Nil<core::T_O>();
          else value = gc::As<gc::smart_ptr<OT>>(oCdr(oCar(find)));
        }
        break;
      case patching: {
          if ( value.notnilp() ) {
            gc::smart_ptr<T_O> orig((gc::Tagged)value.raw_());
            T_sp patch = record_circle_subst(this->_replacement_table,orig);
            if ( patch != orig ) value.rawRef_() = reinterpret_cast<OT*>(patch.raw_());
          }
        }
        break;
      }
    }


    template <typename T>
      void field_if_not_default(Symbol_sp name, T& value, const T& default_value ) {
      switch (this->stage()) {
      case saving: {
          if ( !(value == default_value) ) {
            core::T_sp obj_value = translate::to_object<T>::convert(value);
            this->_alist = Cons_O::create(Cons_O::create(name,obj_value),this->_alist);
          }
        }
        break;
      case loading: {
      // I could speed this up if I cache the entry after this find
      // and search from there and reverse the alist once it's done
          List_sp find = alist_get(this->_alist,name);
          if ( find.nilp() ) {
            value = default_value;
          } else {
            value = translate::from_object<T>(oCdr(oCar(find)))._v;
          }
        }
        break;
      case patching:
          // Do nothing for POD values
          break;
      }
    }
  };

};
#endif
