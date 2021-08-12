//
// Created by Romain Neil on 28/03/2021.
//

#ifndef HTTP_COUNTER_COMPTEUR_H
#define HTTP_COUNTER_COMPTEUR_H

#include <string>

class Compteur {

	public:
		explicit Compteur(std::string  nom, int initialValue = 0);
		~Compteur() = default;

		/**
		 * Get the name of the counter
		 * @return the counter name
		 */
		[[nodiscard]] const std::string &getNom() const {
			return m_nom;
		}

		/**
		 * Get counter value
		 * @return the counter value
		 */
		[[nodiscard]] int getVal() const {
			return m_val;
		}

		/**
		 * Increase the counter value
		 * @param val value, default at 1
		 */
		void inc(int val = 1);

		void setVal(int newVal);

	private:

		std::string m_nom;
		int m_val = 0;

};


#endif //HTTP_COUNTER_COMPTEUR_H
