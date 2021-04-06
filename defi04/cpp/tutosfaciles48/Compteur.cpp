//
// Created by Romain Neil on 28/03/2021.
//

#include "Compteur.h"

Compteur::Compteur(std::string nom, int initialValue) : m_nom(std::move(nom)), m_val(initialValue) {}

void Compteur::inc(int val) {
	m_val += val;
}

void Compteur::setVal(int newVal) {
	m_val = newVal;
}