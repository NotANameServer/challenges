/****************************************************************************
** Meta object code from reading C++ file 'lvlonewindow.h'
**
** Created by: The Qt Meta Object Compiler version 68 (Qt 6.0.1)
**
** WARNING! All changes made in this file will be lost!
*****************************************************************************/

#include <memory>
#include "../AutoCell/lvlonewindow.h"
#include <QtCore/qbytearray.h>
#include <QtCore/qmetatype.h>
#if !defined(Q_MOC_OUTPUT_REVISION)
#error "The header file 'lvlonewindow.h' doesn't include <QObject>."
#elif Q_MOC_OUTPUT_REVISION != 68
#error "This file was generated using the moc from 6.0.1. It"
#error "cannot be used with the include files from this version of Qt."
#error "(The moc has changed too much.)"
#endif

QT_BEGIN_MOC_NAMESPACE
QT_WARNING_PUSH
QT_WARNING_DISABLE_DEPRECATED
struct qt_meta_stringdata_Alive_t {
    const uint offsetsAndSize[6];
    char stringdata0[22];
};
#define QT_MOC_LITERAL(ofs, len) \
    uint(offsetof(qt_meta_stringdata_Alive_t, stringdata0) + ofs), len 
static const qt_meta_stringdata_Alive_t qt_meta_stringdata_Alive = {
    {
QT_MOC_LITERAL(0, 5), // "Alive"
QT_MOC_LITERAL(6, 14), // "fillGridLayout"
QT_MOC_LITERAL(21, 0) // ""

    },
    "Alive\0fillGridLayout\0"
};
#undef QT_MOC_LITERAL

static const uint qt_meta_data_Alive[] = {

 // content:
       9,       // revision
       0,       // classname
       0,    0, // classinfo
       1,   14, // methods
       0,    0, // properties
       0,    0, // enums/sets
       0,    0, // constructors
       0,       // flags
       1,       // signalCount

 // signals: name, argc, parameters, tag, flags, initial metatype offsets
       1,    1,   20,    2, 0x06,    0 /* Public */,

 // signals: parameters
    QMetaType::Void, QMetaType::Int,    2,

       0        // eod
};

void Alive::qt_static_metacall(QObject *_o, QMetaObject::Call _c, int _id, void **_a)
{
    if (_c == QMetaObject::InvokeMetaMethod) {
        auto *_t = static_cast<Alive *>(_o);
        (void)_t;
        switch (_id) {
        case 0: _t->fillGridLayout((*reinterpret_cast< int(*)>(_a[1]))); break;
        default: ;
        }
    } else if (_c == QMetaObject::IndexOfMethod) {
        int *result = reinterpret_cast<int *>(_a[0]);
        {
            using _t = void (Alive::*)(int );
            if (*reinterpret_cast<_t *>(_a[1]) == static_cast<_t>(&Alive::fillGridLayout)) {
                *result = 0;
                return;
            }
        }
    }
}

const QMetaObject Alive::staticMetaObject = { {
    QMetaObject::SuperData::link<QThread::staticMetaObject>(),
    qt_meta_stringdata_Alive.offsetsAndSize,
    qt_meta_data_Alive,
    qt_static_metacall,
    nullptr,
qt_incomplete_metaTypeArray<qt_meta_stringdata_Alive_t
, QtPrivate::TypeAndForceComplete<void, std::false_type>, QtPrivate::TypeAndForceComplete<int, std::false_type>



>,
    nullptr
} };


const QMetaObject *Alive::metaObject() const
{
    return QObject::d_ptr->metaObject ? QObject::d_ptr->dynamicMetaObject() : &staticMetaObject;
}

void *Alive::qt_metacast(const char *_clname)
{
    if (!_clname) return nullptr;
    if (!strcmp(_clname, qt_meta_stringdata_Alive.stringdata0))
        return static_cast<void*>(this);
    return QThread::qt_metacast(_clname);
}

int Alive::qt_metacall(QMetaObject::Call _c, int _id, void **_a)
{
    _id = QThread::qt_metacall(_c, _id, _a);
    if (_id < 0)
        return _id;
    if (_c == QMetaObject::InvokeMetaMethod) {
        if (_id < 1)
            qt_static_metacall(this, _c, _id, _a);
        _id -= 1;
    } else if (_c == QMetaObject::RegisterMethodArgumentMetaType) {
        if (_id < 1)
            *reinterpret_cast<QMetaType *>(_a[0]) = QMetaType();
        _id -= 1;
    }
    return _id;
}

// SIGNAL 0
void Alive::fillGridLayout(int _t1)
{
    void *_a[] = { nullptr, const_cast<void*>(reinterpret_cast<const void*>(std::addressof(_t1))) };
    QMetaObject::activate(this, &staticMetaObject, 0, _a);
}
struct qt_meta_stringdata_LvlOneWindow_t {
    const uint offsetsAndSize[10];
    char stringdata0[62];
};
#define QT_MOC_LITERAL(ofs, len) \
    uint(offsetof(qt_meta_stringdata_LvlOneWindow_t, stringdata0) + ofs), len 
static const qt_meta_stringdata_LvlOneWindow_t qt_meta_stringdata_LvlOneWindow = {
    {
QT_MOC_LITERAL(0, 12), // "LvlOneWindow"
QT_MOC_LITERAL(13, 16), // "onFillGridLayout"
QT_MOC_LITERAL(30, 0), // ""
QT_MOC_LITERAL(31, 15), // "onClickBtnStart"
QT_MOC_LITERAL(47, 14) // "onClickBtnStop"

    },
    "LvlOneWindow\0onFillGridLayout\0\0"
    "onClickBtnStart\0onClickBtnStop"
};
#undef QT_MOC_LITERAL

static const uint qt_meta_data_LvlOneWindow[] = {

 // content:
       9,       // revision
       0,       // classname
       0,    0, // classinfo
       3,   14, // methods
       0,    0, // properties
       0,    0, // enums/sets
       0,    0, // constructors
       0,       // flags
       0,       // signalCount

 // slots: name, argc, parameters, tag, flags, initial metatype offsets
       1,    1,   32,    2, 0x0a,    0 /* Public */,
       3,    0,   35,    2, 0x0a,    2 /* Public */,
       4,    0,   36,    2, 0x0a,    3 /* Public */,

 // slots: parameters
    QMetaType::Void, QMetaType::Int,    2,
    QMetaType::Void,
    QMetaType::Void,

       0        // eod
};

void LvlOneWindow::qt_static_metacall(QObject *_o, QMetaObject::Call _c, int _id, void **_a)
{
    if (_c == QMetaObject::InvokeMetaMethod) {
        auto *_t = static_cast<LvlOneWindow *>(_o);
        (void)_t;
        switch (_id) {
        case 0: _t->onFillGridLayout((*reinterpret_cast< int(*)>(_a[1]))); break;
        case 1: _t->onClickBtnStart(); break;
        case 2: _t->onClickBtnStop(); break;
        default: ;
        }
    }
}

const QMetaObject LvlOneWindow::staticMetaObject = { {
    QMetaObject::SuperData::link<QMainWindow::staticMetaObject>(),
    qt_meta_stringdata_LvlOneWindow.offsetsAndSize,
    qt_meta_data_LvlOneWindow,
    qt_static_metacall,
    nullptr,
qt_incomplete_metaTypeArray<qt_meta_stringdata_LvlOneWindow_t

, QtPrivate::TypeAndForceComplete<void, std::false_type>, QtPrivate::TypeAndForceComplete<int, std::false_type>, QtPrivate::TypeAndForceComplete<void, std::false_type>, QtPrivate::TypeAndForceComplete<void, std::false_type>


>,
    nullptr
} };


const QMetaObject *LvlOneWindow::metaObject() const
{
    return QObject::d_ptr->metaObject ? QObject::d_ptr->dynamicMetaObject() : &staticMetaObject;
}

void *LvlOneWindow::qt_metacast(const char *_clname)
{
    if (!_clname) return nullptr;
    if (!strcmp(_clname, qt_meta_stringdata_LvlOneWindow.stringdata0))
        return static_cast<void*>(this);
    return QMainWindow::qt_metacast(_clname);
}

int LvlOneWindow::qt_metacall(QMetaObject::Call _c, int _id, void **_a)
{
    _id = QMainWindow::qt_metacall(_c, _id, _a);
    if (_id < 0)
        return _id;
    if (_c == QMetaObject::InvokeMetaMethod) {
        if (_id < 3)
            qt_static_metacall(this, _c, _id, _a);
        _id -= 3;
    } else if (_c == QMetaObject::RegisterMethodArgumentMetaType) {
        if (_id < 3)
            *reinterpret_cast<QMetaType *>(_a[0]) = QMetaType();
        _id -= 3;
    }
    return _id;
}
QT_WARNING_POP
QT_END_MOC_NAMESPACE
